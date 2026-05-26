unit Mundus.Renderer.Worker;

interface

uses
  Classes,
  VCL.Graphics,
  SyncObjs,
  Generics.Collections,
  Mundus.DrawCall,
  Mundus.Shader.Texture,
  Mundus.Math,
  Mundus.Types,
  Mundus.Diagnostics.StopWatch,
  Mundus.FrameBuffer,
  Mundus.Renderer.Worker.Sync,
  Mundus.Renderer.Worker.Buffer,
  Mundus.GeometryBuffer;

type
  TRenderWorker = class(TThread)
  private
    FDrawCalls: TDrawCalls;
    FDone: TEvent;
    FStart: TEvent;
    FBlockSteps: Integer;
    FBlockOffset: Integer;
    FResolutionX: Integer;
    FResolutionY: Integer;
    FMaxResolutionX: Integer;
    FMaxResolutionY: Integer;
    FHalfResolutionX: Integer;
    FHalfResolutionY: Integer;
    FWatch: TStopWatch;
    FFrameBuffer: TFrameBuffer;
    FBlockEnd: Integer;
    FVectorPassSync: TStageSync;
    FBuffers: PRenderWorkerBuffers;
    procedure SetResolutionX(const Value: Integer);
    procedure SetResolutionY(const Value: Integer);
    function GetFPS: Integer;
    function GetRenderFence: THandle;
    procedure PrepareBuffer(const ASource: PDrawCall; const ATarget: PRenderWorkerBuffer);
    procedure RunRasterization;
    procedure RunVectorization;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(AVectorPassSync: TStageSync);
    destructor Destroy; override;
    procedure StartRender;
    property DrawCalls: TDrawCalls read FDrawCalls write FDrawCalls;
    property BlockSteps: Integer read FBlockSteps write FBlockSteps;
    property BlockOffset: Integer read FBlockOffset write FBlockOffset;
    property BlockEnd:Integer read FBlockEnd write FBlockEnd;
    property ResolutionX: Integer read FResolutionX write SetResolutionX;
    property ResolutionY: Integer read FResolutionY write SetResolutionY;
    property FrameBuffer: TFrameBuffer read FFrameBuffer write FFrameBuffer;
    property FPS: Integer read GetFPS;
    property RenderFence: THandle read GetRenderFence;
    property Buffer: PRenderWorkerBuffers read FBuffers write FBuffers;
  end;

implementation

uses
  Windows,
  Mundus.Shader,
  Mundus.Renderer.Clipping,
  System.Math;

{ TRenderWorker }

constructor TRenderWorker.Create;
begin
  inherited Create(True);
  FDone := TEvent.Create(nil, False, True, '');
  FStart := TEvent.Create(nil, False, False, '');
  FWatch := TStopWatch.Create();
  FVectorPassSync := AVectorPassSync;
end;

destructor TRenderWorker.Destroy;
begin
  inherited;
  FStart.Free;
  FDone.Free;
  FWatch.Free;
end;

procedure TRenderWorker.Execute;
begin
  NameThreadForDebugging('RenderWorker');
  while not Terminated do
  begin
    FStart.WaitFor();
    FWatch.Start;
    if Assigned(FDrawCalls) then
    begin
      RunVectorization;
      RunRasterization;
    end;
    FWatch.Stop;
    FDone.SetEvent;
  end;
end;

function TRenderWorker.GetFPS: Integer;
var
  LMicro: Int64;
begin
  LMicro := FWatch.ElapsedMicroseconds;
  if LMicro > 0 then
    Result := 1000000 div LMicro
  else
    Result := 10000;
end;

function TRenderWorker.GetRenderFence: THandle;
begin
  Result := FDone.Handle;
end;

procedure TRenderWorker.PrepareBuffer(const ASource: PDrawCall; const ATarget: PRenderWorkerBuffer);
var
  LClipContext: TClipContext;
  i, LCount: Integer;
  LVertex: TFloat4;
  LVInput, LUniformInput: PByte;
  LMinY, LMaxY: Single;
  LGeometry: PGeometryBuffer;

  procedure ProcessTriangle(A, B, C: Integer);
  var
    LA, LB, LC: TFloat4;
    LNormal: TFloat3;
    LClippedTriangle: array[0..2] of Integer;
    i: Integer;
  begin
    ClipPolygon(ATarget, @LClipContext, A, B, C);
    //if less than 3, it is fully clipped
    if LClipContext.ResultBuffer.Count >= 3 then
    begin
      LClippedTriangle[0] := LClipContext.ResultBuffer.Indices[0];
      LClippedTriangle[1] := LClipContext.ResultBuffer.Indices[1];
      LClippedTriangle[2] := LClipContext.ResultBuffer.Indices[2];
      LA := ATarget.Vertices[LClippedTriangle[0]];
      LA.XYZ := LA.XYZ / LA.W;
      LB := ATarget.Vertices[LClippedTriangle[1]];
      LB.XYZ := LB.XYZ / LB.W;
      LC := ATarget.Vertices[LClippedTriangle[2]];
      LC.XYZ := LC.XYZ / LC.W;
      LNormal := CalculateSurfaceNormal(LA.XYZ, LB.XYZ, LC.XYZ);
      //Backface culling
      if LNormal.Z < 0 then
      begin
        ATarget.AddIndices(LClippedTriangle);
        for i := 3 to Pred(LClipContext.ResultBuffer.Count) do
        begin
          //we connect 2 new vertices to our vertex at index 0
          LClippedTriangle[1] := LClipContext.ResultBuffer.Indices[i-1];
          LClippedTriangle[2] := LClipContext.ResultBuffer.Indices[i];
          ATarget.AddIndices(LClippedTriangle);
        end;
      end;
    end;
  end;

begin
  ATarget.Reset;

  if Assigned(ASource.Geometry.Shader) then
  begin
    LGeometry := @ASource.Geometry;
    LClipContext := TClipContext.Create();
    LUniformInput := @LGeometry.UniformValues.Data[0];
    LVInput := @LGeometry.Values.Data[0];
    ATarget.InitBuffers(Length(LGeometry.Vertices), Length(LGeometry.VertexIndices), LGeometry.Shader.FragmentAttributeSize);
    for i := 0 to High(LGeometry.Vertices) do
    begin
      LVertex.XYZ := LGeometry.Vertices[i];
      LVertex.W := 1;
      LGeometry.Shader.VertexShader(LVertex, LUniformInput, LVInput, ATarget.Attributes[i]);
      ATarget.Vertices[i] := LVertex;
      Inc(LVInput, LGeometry.Shader.VertexBufferDescriptor.RecordSize);
    end;

    if Assigned(LGeometry.VertexIndices) then
    begin
      LCount := Length(LGeometry.VertexIndices) div 3;
      for i := 0 to Pred(LCount) do
        ProcessTriangle(LGeometry.VertexIndices[i * 3], LGeometry.VertexIndices[i * 3 + 1], LGeometry.VertexIndices[i * 3 + 2]);
    end
    else
    begin
      LCount := ATarget.VertexCount div 3;
      for i := 0 to Pred(LCount) do
        ProcessTriangle(i * 3, i * 3 + 1, i * 3 + 2);
    end;
  end;

  LMaxY := 0;
  LMinY := FResolutionY;
  for i := 0 to Pred(ATarget.VertexCount) do
  begin
    LVertex := ATarget.Vertices[i];
    LVertex.XYZ := LVertex.XYZ / LVertex.W;
    //denormalize vectors to screenpos
    LVertex.X := (1-LVertex.X) * FHalfResolutionX;//half screen size
    LVertex.Y := (1-LVertex.Y) * FHalfResolutionY;

    if LVertex.Y < LMinY then
      LMinY := LVertex.Y
    else if LVertex.Y > LMaxY then
      LMaxY := LVertex.Y;

    ATarget.Vertices[i] := LVertex;
  end;
  ATarget.MinY := Trunc(Max(0, LMinY));
  ATarget.MaxY := Trunc(Min(FResolutionY, LMaxY));
end;

{$PointerMath ON}
procedure TRenderWorker.RunRasterization;
var
  LCall: PDrawCall;
  LBuffer: PRenderWorkerBuffer;
  LTriangle: PInteger;
  i, k: Integer;
  LVertexA, LVertexB, LVertexC: TFloat4;
  LRasterizer: TRasterizer;
  LRenderTarget: Pointer;
  LFirstDepth, LFirstLowDepth: System.PSingle;
  LMinY, LMaxY: Integer;
  LTriangleCount: Integer;
  LGeometry: PGeometryBuffer;
begin
  LRenderTarget := FFrameBuffer.FirstPixel;
  LFirstDepth := FFrameBuffer.DepthBuffer;
  LFirstLowDepth := FFrameBuffer.LowDepthBuffer;
  LMinY := FBlockOffset * CQuadSize;
  LMaxY := FBlockEnd * CQuadSize;
  for i := 0 to Pred(FDrawCalls.Count) do
  begin
    LCall := FDrawCalls[i];
    LBuffer := @FBuffers.Buffers[i];
    if (LBuffer.MinY > LMaxY) or (LBuffer.MaxY < LMinY) then
      Continue;

    LGeometry := @LCall.Geometry;
    LRasterizer := LGeometry.Shader.Rasterizer;
    LTriangleCount := LBuffer.IndexCount div 3;
    LTriangle := @LBuffer.Indices[0];
    for k := 0 to Pred(LTriangleCount) do
    begin
      LVertexA := LBuffer.Vertices[LTriangle[0]];
      LVertexB := LBuffer.Vertices[LTriangle[1]];
      LVertexC := LBuffer.Vertices[LTriangle[2]];

      //check if triangle overlaps with workers render area. Skip if not intersecting
      if ((LVertexA.Y > LMaxY) and (LVertexB.Y > LMaxY) and (LVertexC.Y > LMaxY))
        or ((LVertexA.Y < LMinY) and (LVertexB.Y < LMinY) and (LVertexC.Y < LMinY))
      then
      begin
        Inc(LTriangle, 3);
        Continue;
      end;

      LRasterizer(
        FMaxResolutionX, FMaxResolutionY,
        LVertexA, LVertexB, LVertexC,
        LBuffer.Attributes[LTriangle[0]],
        LBuffer.Attributes[LTriangle[1]],
        LBuffer.Attributes[LTriangle[2]],
        @LGeometry.UniformValues.Data[0],
        LRenderTarget,
        LFirstDepth,
        LFirstLowDepth,
        FBlockOffset, FBlockSteps, FBlockEnd);
      Inc(LTriangle, 3);
    end;
  end;
end;

procedure TRenderWorker.RunVectorization;
var
  LIndex: Integer;
begin
  while FBuffers.TryGetNextUnprepared(LIndex) do
    PrepareBuffer(FDrawCalls.Calls[LIndex], @FBuffers.Buffers[LIndex]);
  FVectorPassSync.Sync;
end;

procedure TRenderWorker.SetResolutionX(const Value: Integer);
begin
  FResolutionX := Value;
  FMaxResolutionX := Value - 1;
  FHalfResolutionX := Value div 2;
end;

procedure TRenderWorker.SetResolutionY(const Value: Integer);
begin
  FResolutionY := Value;
  FMaxResolutionY := Value - 1;
  FHalfResolutionY := Value div 2;
end;

procedure TRenderWorker.StartRender;
begin
  FStart.SetEvent;
end;

procedure TRenderWorker.TerminatedSet;
begin
  inherited;
  FStart.SetEvent;
end;

end.
