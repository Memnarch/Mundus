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
  Mundus.Renderer.Worker.Sync;

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
    FTempAttributes: TVertexAttributeBuffer;
    procedure SetResolutionX(const Value: Integer);
    procedure SetResolutionY(const Value: Integer);
    function GetFPS: Integer;
    function GetRenderFence: THandle;
    procedure PrepareDrawcall(const ATarget: PDrawCall);
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
  end;

implementation

uses
  Windows,
  Mundus.Shader,
  Mundus.Renderer.Clipping;

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

procedure TRenderWorker.PrepareDrawcall(const ATarget: PDrawCall);
var
  LClipContext: TClipContext;
  i, LCount: Integer;
  LVertex: TFloat4;
  LVInput, LUniformInput: PByte;

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
        ATarget.AddProcessedIndices(LClippedTriangle);
        for i := 3 to Pred(LClipContext.ResultBuffer.Count) do
        begin
          LClippedTriangle[0] := LClipContext.ResultBuffer.Indices[0];
          LClippedTriangle[1] := LClipContext.ResultBuffer.Indices[i-1];
          LClippedTriangle[2] := LClipContext.ResultBuffer.Indices[i];
          ATarget.AddProcessedIndices(LClippedTriangle);
        end;
      end;
    end;
  end;

begin
  if Assigned(ATarget.Shader) then
  begin
    LClipContext := TClipContext.Create();
    if Length(FTempAttributes) < ATarget.Shader.FragmentAttributeSize then
      SetLength(FTempAttributes, ATarget.Shader.FragmentAttributeSize);
    LUniformInput := @ATarget.ConstantValues[0];
    LVInput := @ATarget.Values[0];
    for i := 0 to Pred(ATarget.VertexCount) do
    begin
      LVertex := ATarget.Vertices[i];
      ATarget.Shader.VertexShader(LVertex, LUniformInput, LVInput, FTempAttributes);
      ATarget.UpdateVertex(i, LVertex, @FTempAttributes[0]);
      Inc(LVInput, ATarget.Shader.VertexBufferDescriptor.RecordSize);
    end;

    if Assigned(ATarget.VertexIndices) then
    begin
      LCount := Length(ATarget.VertexIndices) div 3;
      for i := 0 to Pred(LCount) do
        ProcessTriangle(ATarget.VertexIndices[i * 3], ATarget.VertexIndices[i * 3 + 1], ATarget.VertexIndices[i * 3 + 2]);
    end
    else
    begin
      LCount := Length(ATarget.Vertices) div 3;
      for i := 0 to Pred(LCount) do
        ProcessTriangle(i * 3, i * 3 + 1, i * 3 + 2);
    end;
  end;

  for i := 0 to Pred(ATarget.VertexCount) do
  begin
    LVertex := ATarget.Vertices[i];
    LVertex.XYZ := LVertex.XYZ / LVertex.W;
    //denormalize vectors to screenpos
    LVertex.X := (1-LVertex.X) * FHalfResolutionX;//half screen size
    LVertex.Y := (1-LVertex.Y) * FHalfResolutionY;
    ATarget.Vertices[i] := LVertex;
  end;
end;

{$PointerMath ON}
procedure TRenderWorker.RunRasterization;
var
  LCall: PDrawCall;
  LTriangle: PInteger;
  i, k: Integer;
  LVertexA, LVertexB, LVertexC: TFloat4;
  LRasterizer: TRasterizer;
  LRenderTarget: Pointer;
  LFirstDepth, LFirstLowDepth: System.PSingle;
  LMinY, LMaxY: Integer;
  LTriangleCount: Integer;
begin
  LRenderTarget := FFrameBuffer.FirstPixel;
  LFirstDepth := FFrameBuffer.DepthBuffer;
  LFirstLowDepth := FFrameBuffer.LowDepthBuffer;
  LMinY := FBlockOffset * CQuadSize;
  LMaxY := FBlockEnd * CQuadSize;
  for i := 0 to Pred(FDrawCalls.Count) do
  begin
    LCall := FDrawCalls[i];
    LRasterizer := LCall.Shader.Rasterizer;
    LTriangleCount := LCall.ProcessedIndicesCount div 3;
    for k := 0 to Pred(LTriangleCount) do
    begin
      LTriangle := @LCall.ProcessedIndices[k * 3];
      LVertexA := LCall.Vertices[LTriangle[0]];
      LVertexB := LCall.Vertices[LTriangle[1]];
      LVertexC := LCall.Vertices[LTriangle[2]];
//
      //check if triangle overlaps with workers render area. Skip if not intersecting
      if ((LVertexA.Y > LMaxY) and (LVertexB.Y > LMaxY) and (LVertexC.Y > LMaxY))
        or ((LVertexA.Y < LMinY) and (LVertexB.Y < LMinY) and (LVertexC.Y < LMinY))
      then
        Continue;

      LRasterizer(
        FMaxResolutionX, FMaxResolutionY,
        LVertexA, LVertexB, LVertexC,
        LCall.Attributes[LTriangle[0]],
        LCall.Attributes[LTriangle[1]],
        LCall.Attributes[LTriangle[2]],
        @LCall.ConstantValues[0],
        LRenderTarget,
        LFirstDepth,
        LFirstLowDepth,
        FBlockOffset, FBlockSteps, FBlockEnd);
    end;
  end;
end;

procedure TRenderWorker.RunVectorization;
var
  LCall: PDrawCall;
begin
  while FDrawCalls.TryGetUnpreparedCall(LCall) do
    PrepareDrawcall(LCall);
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
