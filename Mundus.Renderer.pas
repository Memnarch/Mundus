unit Mundus.Renderer;

interface

uses
  Types,
  Classes,
  Windows,
  SysUtils,
  Graphics,
  Generics.Collections,
  Mundus.Math,
  Mundus.Types,
  Mundus.Shader,
  Mundus.Diagnostics.StopWatch,
  Mundus.DrawCall,
  Mundus.Renderer.Worker,
  Mundus.Camera,
  Mundus.ValueBuffer,
  Mundus.FrameBuffer,
  Mundus.GeometryBuffer;

type
  TRenderEvent = procedure(Canvas: TCanvas) of object;

  TMundusRenderer = class
  private
    FBackBuffer: array[Boolean] of TFrameBuffer;
    FDrawCalls: array[Boolean] of TDrawCalls;
    FGeometryBuffers: TGeometryBuffers;
    FFPS: Integer;
    FResolutionX: Integer;
    FResolutionY: Integer;
    FOnAfterFrame: TRenderEvent;
    FTimer: TStopWatch;
    FWorkers: TObjectList<TRenderWorker>;
    FRenderFences: TArray<THandle>;
    FCurrentBuffer: Boolean;
    FWorkerFPS: Integer;
    procedure ProcessGeometry(const AGeometry: PGeometryBuffer; const ATarget: PDrawCall);
    procedure DoAfterFrame(ACanvas: TCanvas);
    function GenerateDrawCalls: PDrawCalls;
    procedure DispatchCalls(ACanvas: TCanvas; ACalls: PDrawCalls);
    procedure SpinupWorkers(AWorkerCount: Integer);
    procedure TerminateWorkers;
    procedure WaitForRender;
    procedure UpdateBufferResolution(ABuffer: Boolean; AWidth, AHeight: Integer);
    procedure ClearBuffer(ABuffer: Boolean);
    function GetRenderWorkers: Integer;
    procedure AssignWorkerAreas(const ABufferHeight: Integer);
  public
    constructor Create(AWorker: Integer = 1);
    destructor Destroy(); override;
    procedure SetResolution(AWidth, AHeight: Integer);
    function NewFrame: PGeometryBuffers;
    procedure RenderFrame(ACanvas: TCanvas);
    function GetCurrentFPS(): Integer;
    property OnAfterFrame: TRenderEvent read FOnAfterFrame write FOnAfterFrame;
    property ResolutionX: Integer read FResolutionX;
    property ResolutionY: Integer read FResolutionY;
    property ReenderWorkers: Integer read GetRenderWorkers;
  end;

  function RGB32(ARed, AGreen, ABlue, AAlpha: Byte): TRGB32;

implementation

uses
  Math,
  DateUtils,
  Mundus.Shader.VertexGradient,
  Mundus.Shader.DepthColor,
  Mundus.Shader.Texture,
  Mundus.Renderer.Clipping;

{ TSoftwareRenderer }

procedure TMundusRenderer.AssignWorkerAreas(const ABufferHeight: Integer);
var
  LRows, LRowsPerWorker, LMissingRows: Integer;
  i, LOffset, LBlockEnd: Integer;
  LWorker: TRenderWorker;
begin
  LRows := ABufferHeight div CQuadSize;
  LRowsPerWorker := LRows div FWorkers.Count;
  LMissingRows := LRows mod FWorkers.Count;
  LBlockEnd := 0;
  for i := 0 to Pred(FWorkers.Count) do
  begin
    LOffset := LBlockEnd;
    LBlockEnd := LOffset + LRowsPerWorker;
    if LMissingRows > 0 then
    begin
      Inc(LBlockEnd);
      Dec(LMissingRows);
    end;
    LWorker := FWorkers[i];
    LWorker.BlockSteps := 1;
    LWorker.BlockOffset := LOffset;
    LWorker.BlockEnd := LBlockEnd;
  end;
end;

procedure TMundusRenderer.ClearBuffer(ABuffer: Boolean);
begin
  FBackBuffer[ABuffer].Clear;
end;

constructor TMundusRenderer.Create;
begin
  FBackBuffer[True] := TFrameBuffer.Create();
  FBackBuffer[False] := TFrameBuffer.Create();
  FDrawCalls[True] := TDrawCalls.Create();
  FDrawCalls[False] := TDrawCalls.Create();
  SetResolution(512, 512);

  FTimer := TStopWatch.Create(False);

  FWorkers := TObjectList<TRenderWorker>.Create();
  SpinupWorkers(AWorker);
end;

destructor TMundusRenderer.Destroy;
begin
  TerminateWorkers;
  FWorkers.Free;
  FBackBuffer[True].Free();
  FBackBuffer[False].Free();
  FDrawCalls[True].Free;
  FDrawCalls[False].Free;
  FTimer.Free;
  inherited;
end;

procedure TMundusRenderer.DispatchCalls(ACanvas: TCanvas; ACalls: PDrawCalls);
var
  LWorker: TRenderWorker;
  LBackBuffer, LFrontBuffer: Boolean;
  LFPS: Integer;
begin
  LBackBuffer := FCurrentBuffer;
  LFrontBuffer := not FCurrentBuffer;

  //ResetBackBuffer from last frame
  UpdateBufferResolution(LFrontBuffer, FResolutionX, FResolutionY);
  ClearBuffer(LFrontBuffer);

  //wait for workers to finish frame
  WaitForRender;
  AssignWorkerAreas(FResolutionY);
  //load workers with new stuff and start
  FWorkerFPS := High(FWorkerFPS);
  for LWorker in FWorkers do
  begin
    LWorker.DrawCalls := ACalls^;
    LWorker.FrameBuffer := FBackBuffer[LFrontBuffer];
    LWorker.ResolutionX := FResolutionX;
    LWorker.ResolutionY := FResolutionY;
    LFPS := LWorker.FPS;
    if LFPS < FWorkerFPS then
      FWorkerFPS := LFPS;
    LWorker.StartRender;
  end;

  //Draw Backbuffer to FrontBuffer
  FBackBuffer[LBackBuffer].Draw(ACanvas, ACanvas.ClipRect);
  DoAfterFrame(ACanvas);
  //flip buffers
  FCurrentBuffer := not FCurrentBuffer;
end;

procedure TMundusRenderer.DoAfterFrame(ACanvas: TCanvas);
begin
  if Assigned(FOnAfterFrame) then
  begin
    FOnAfterFrame(ACanvas);
  end;
end;

function TMundusRenderer.GenerateDrawCalls: PDrawCalls;
var
  LGeometries: PGeometryBuffers;
  i: Integer;
begin
  Result := @FDrawCalls[not FCurrentBuffer];
  Result.Reset;
  LGeometries := @FGeometryBuffers;
  for i := 0 to Pred(LGeometries.Count) do
    ProcessGeometry(LGeometries.Geometries[i], Result.Add());
end;

function TMundusRenderer.GetCurrentFPS: Integer;
begin
  Result := FFPS;
end;

function TMundusRenderer.GetRenderWorkers: Integer;
begin
  Result := FWorkers.Count;
end;

function TMundusRenderer.NewFrame: PGeometryBuffers;
begin
  Result := @FGeometryBuffers;
  Result.Clear;
end;

procedure TMundusRenderer.ProcessGeometry(const AGeometry: PGeometryBuffer; const ATarget: PDrawCall);
var
  LClipContext: TClipContext;
  i, LCount: Integer;
  LVertex: TFloat4;
  LAttributes: TVertexAttributeBuffer;
  LVInput, LUniformInput: PByte;

  procedure ProcessTriangle(A, B, C: Integer);
  var
    LA, LB, LC: TFloat4;
    LNormal: TFloat3;
    LClippedTriangle: TTriangle;
    i: Integer;
  begin
    ClipPolygon(ATarget, @LClipContext, A, B, C);
    //if less than 3, it is fully clipped
    if LClipContext.ResultBuffer.Count >= 3 then
    begin
      LClippedTriangle.VertexA := LClipContext.ResultBuffer.Indices[0];
      LClippedTriangle.VertexB := LClipContext.ResultBuffer.Indices[1];
      LClippedTriangle.VertexC := LClipContext.ResultBuffer.Indices[2];
      LA := ATarget.Vertices[LClippedTriangle.VertexA];
      LA.XYZ := LA.XYZ / LA.W;
      LB := ATarget.Vertices[LClippedTriangle.VertexB];
      LB.XYZ := LB.XYZ / LB.W;
      LC := ATarget.Vertices[LClippedTriangle.VertexC];
      LC.XYZ := LC.XYZ / LC.W;
      LNormal := CalculateSurfaceNormal(LA.XYZ, LB.XYZ, LC.XYZ);
      //Backface culling
      if LNormal.Z < 0 then
      begin
        ATarget.AddTriangle(@LClippedTriangle);
        for i := 3 to Pred(LClipContext.ResultBuffer.Count) do
        begin
          LClippedTriangle.VertexA := LClipContext.ResultBuffer.Indices[0];
          LClippedTriangle.VertexB := LClipContext.ResultBuffer.Indices[i-1];
          LClippedTriangle.VertexC := LClipContext.ResultBuffer.Indices[i];
          ATarget.AddTriangle(@LClippedTriangle);
        end;
      end;
    end;
  end;

begin
  ATarget.Shader := AGeometry.Shader;
  if Assigned(ATarget.Shader) then
  begin
    LClipContext := TClipContext.Create();
    SetLength(LAttributes, ATarget.Shader.FragmentAttributeSize);
    LUniformInput := @AGeometry.UniformValues.Data[0];
    LVInput := @AGeometry.Values.Data[0];
    ATarget.ConstantValues := AGeometry.UniformValues.Data;
    for i := 0 to High(AGeometry.Vertices) do
    begin
      LVertex.XYZ := AGeometry.Vertices[i];
      LVertex.W := 1;
      ATarget.Shader.VertexShader(LVertex, LUniformInput, LVInput, LAttributes);
      ATarget.AddVertex(LVertex, @LAttributes[0]);
      Inc(LVInput, AGeometry.Values.Descriptor.RecordSize);
    end;

    if Assigned(AGeometry.VertexIndices) then
    begin
      LCount := Length(AGeometry.VertexIndices) div 3;
      for i := 0 to Pred(LCount) do
        ProcessTriangle(AGeometry.VertexIndices[i * 3], AGeometry.VertexIndices[i * 3 + 1], AGeometry.VertexIndices[i * 3 + 2]);
    end
    else
    begin
      LCount := Length(AGeometry.Vertices) div 3;
      for i := 0 to Pred(LCount) do
        ProcessTriangle(i * 3, i * 3 + 1, i * 3 + 2);
    end;
  end;

  for i := 0 to High(ATarget.Vertices) do
    ATarget.Vertices[i].XYZ := ATarget.Vertices[i].XYZ / ATarget.Vertices[i].W;
end;

procedure TMundusRenderer.RenderFrame(ACanvas: TCanvas);
var
  LDrawCalls: PDrawCalls;
  LMicro: UInt64;
begin
  FTimer.Start();

  LDrawCalls := GenerateDrawCalls();
  DispatchCalls(ACanvas, LDrawCalls);

  FTimer.Stop();
  LMicro := FTimer.ElapsedMicroseconds;
  if LMicro > 0 then
    FFPS := Min(FWorkerFPS, 1000000 div LMicro)
  else
    FFPS := FWorkerFPS;
end;

procedure TMundusRenderer.SetResolution(AWidth, AHeight: Integer);
begin
  FResolutionX := AWidth div CQuadSize * CQuadSize;
  FResolutionY := AHeight div CQuadSize * CQuadSize;
end;

procedure TMundusRenderer.SpinupWorkers(AWorkerCount: Integer);
var
  i: Integer;
  LWorker: TRenderWorker;
begin
  SetLength(FRenderFences, AWorkerCount);
  for i := 0 to Pred(AWorkerCount) do
  begin
    LWorker := TRenderWorker.Create();
    LWorker.BlockSteps := AWorkerCount;
    LWorker.BlockOffset := i;
    FWorkers.Add(LWorker);
    FRenderFences[i] := LWorker.RenderFence;
    LWorker.Start;
  end;
end;

procedure TMundusRenderer.TerminateWorkers;
var
  LWorker: TRenderWorker;
begin
  for LWorker in FWorkers do
    LWorker.Terminate;
end;

procedure TMundusRenderer.UpdateBufferResolution(ABuffer: Boolean; AWidth, AHeight: Integer);
var
  LBuffer: TFrameBuffer;
begin
  LBuffer := FBackBuffer[ABuffer];
  if (LBuffer.Width <> AWidth) or (LBuffer.Height <> AHeight) then
    LBuffer.Resize(AWidth, AHeight);
end;

procedure TMundusRenderer.WaitForRender;
begin
  WaitForMultipleObjects(Length(FRenderFences), @FRenderFences[0], True, INFINITE);
end;

{ some functions }

function RGB32(ARed, AGreen, ABlue, AAlpha: Byte): TRGB32;
begin
  Result.R := ARed;
  Result.G := AGreen;
  Result.B := ABlue;
  Result.A := AAlpha;
end;

end.
