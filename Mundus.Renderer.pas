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
  Mundus.Mesh,
  Mundus.Types,
  Mundus.Shader,
  Mundus.Diagnostics.StopWatch,
  Mundus.DrawCall,
  Mundus.Renderer.Worker,
  Mundus.Camera,
  Mundus.ValueBuffer,
  Mundus.FrameBuffer;

type
  TRenderEvent = procedure(Canvas: TCanvas) of object;
  TInitBufferEvent = reference to procedure(AMesh: TMesh; const AConstantBuffer, AVertexBuffer: PValueBuffer);

  TMundusRenderer = class
  private
    FBackBuffer: array[boolean] of TFrameBuffer;
    FDrawCalls: array[boolean] of TDrawCalls;
    FMeshList: TObjectList<TMesh>;
    FFPS: Integer;
    FResolutionX: Integer;
    FResolutionY: Integer;
    FOnAfterFrame: TRenderEvent;
    FTimer: TStopWatch;
    FWorkers: TObjectList<TRenderWorker>;
    FRenderFences: TArray<THandle>;
    FCurrentBuffer: Boolean;
    FWorkerFPS: Integer;
    FOnInitValueBuffer: TInitBufferEvent;
    procedure TransformMesh(AMesh: TMesh; ATargetCall: PDrawCall);
    procedure DoAfterFrame(ACanvas: TCanvas);
    function GenerateDrawCalls: TDrawCalls;
    procedure DispatchCalls(ACanvas: TCanvas; ACalls: TDrawCalls);
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
    procedure RenderFrame(ACanvas: TCanvas);
    function GetCurrentFPS(): Integer;
    property MeshList: TObjectList<TMesh> read FMeshList;
    property OnAfterFrame: TRenderEvent read FOnAfterFrame write FOnAfterFrame;
    property ResolutionX: Integer read FResolutionX;
    property ResolutionY: Integer read FResolutionY;
    property ReenderWorkers: Integer read GetRenderWorkers;
    property OnInitValueBuffer: TInitBufferEvent read FOnInitValueBuffer write FOnInitValueBuffer;
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
  FMeshList := TObjectList<TMesh>.Create(False);

  FTimer := TStopWatch.Create(False);

  FWorkers := TObjectList<TRenderWorker>.Create();
  SpinupWorkers(AWorker);
end;

destructor TMundusRenderer.Destroy;
begin
  TerminateWorkers;
  FWorkers.Free;
  FMeshList.Free;
  FBackBuffer[True].Free();
  FBackBuffer[False].Free();
  FDrawCalls[True].Free;
  FDrawCalls[False].Free;
  FTimer.Free;
  inherited;
end;

procedure TMundusRenderer.DispatchCalls(ACanvas: TCanvas; ACalls: TDrawCalls);
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
    LWorker.DrawCalls := ACalls;
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

function TMundusRenderer.GenerateDrawCalls: TDrawCalls;
var
  LMesh: TMesh;
  LCall: PDrawCall;
begin
  Result := FDrawCalls[not FCurrentBuffer];
  Result.Reset;

  for LMesh in FMeshList do
  begin
    LCall := Result.Add;

    LCall.Shader := LMesh.Shader;
    TransformMesh(LMesh, LCall);
  end;
end;

function TMundusRenderer.GetCurrentFPS: Integer;
begin
  Result := FFPS;
end;

function TMundusRenderer.GetRenderWorkers: Integer;
begin
  Result := FWorkers.Count;
end;

procedure TMundusRenderer.RenderFrame(ACanvas: TCanvas);
var
  LDrawCalls: TDrawCalls;
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

procedure TMundusRenderer.TransformMesh(AMesh: TMesh; ATargetCall: PDrawCall);

var
  i, k: Integer;
  LVertex: TFloat4;
  LTriangle: PTriangle;
  LBuffer: TVertexAttributeBuffer;
  LBufferSize: Integer;
  LVInput: PByte;
  LClipContext: TClipContext;
  LClippedTriangle: TTriangle;
  LA, LB, LC: TFloat4;LNormal: TFloat3;
begin
  LBufferSize := AMesh.Shader.FragmentAttributeSize;
  SetLength(LBuffer, LBufferSize);
  ATargetCall.ConstantValues.Initialize(AMesh.Shader.ConstantBufferDescriptor, 1);
  ATargetCall.Values.Initialize(AMesh.Shader.VertexBufferDescriptor, Length(AMesh.Vertices));
  if Assigned(FOnInitValueBuffer) then
    FOnInitValueBuffer(AMesh, @ATargetCall.ConstantValues, @ATargetCall.Values);

  //transform all vertices
  LVInput := @ATargetCall.Values.Data[0];
  for i := 0 to High(AMesh.Vertices) do
  begin
    LVertex.XYZ := AMesh.Vertices[i];
    LVertex.W := 1;
    ATargetCall.Shader.VertexShader(LVertex, @ATargetCall.ConstantValues.Data[0], LVInput, @LBuffer[0]);
    ATargetCall.AddVertex(LVertex, @LBuffer[0]);
    Inc(LVInput, ATargetCall.Values.Descriptor.RecordSize);
  end;

  //add visible triangles
  LClipContext := TClipContext.Create();
  for i := 0 to High(AMesh.Triangles) do
  begin
    LTriangle := @AMesh.Triangles[i];
    ClipPolygon(ATargetCall, @LClipContext, LTriangle.VertexA, LTriangle.VertexB, LTriangle.VertexC);
    //if less than 3, it is fully clipped
    if LClipContext.ResultBuffer.Count >= 3 then
    begin
      LClippedTriangle.VertexA := LClipContext.ResultBuffer.Indices[0];
      LClippedTriangle.VertexB := LClipContext.ResultBuffer.Indices[1];
      LClippedTriangle.VertexC := LClipContext.ResultBuffer.Indices[2];
      LA := ATargetCall.Vertices[LClippedTriangle.VertexA];
      LA.XYZ := LA.XYZ / LA.W;
      LB := ATargetCall.Vertices[LClippedTriangle.VertexB];
      LB.XYZ := LB.XYZ / LB.W;
      LC := ATargetCall.Vertices[LClippedTriangle.VertexC];
      LC.XYZ := LC.XYZ / LC.W;
      LNormal := CalculateSurfaceNormal(LA.XYZ, LB.XYZ, LC.XYZ);
      //Backface culling
      if LNormal.Z < 0 then
      begin
        ATargetCall.AddTriangle(@LClippedTriangle);
        for k := 3 to Pred(LClipContext.ResultBuffer.Count) do
        begin
          LClippedTriangle.VertexA := LClipContext.ResultBuffer.Indices[0];
          LClippedTriangle.VertexB := LClipContext.ResultBuffer.Indices[k-1];
          LClippedTriangle.VertexC := LClipContext.ResultBuffer.Indices[k];
          ATargetCall.AddTriangle(@LClippedTriangle);
        end;
      end;
    end;
  end;

  for i := 0 to High(ATargetCall.Vertices) do
    ATargetCall.Vertices[i].XYZ := ATargetCall.Vertices[i].XYZ / ATargetCall.Vertices[i].W;
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
