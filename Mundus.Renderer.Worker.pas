unit Mundus.Renderer.Worker;

interface

uses
  Classes,
  Graphics,
  SyncObjs,
  Generics.Collections,
  Mundus.DrawCall,
  Mundus.Shader.Texture,
  Mundus.Math,
  Mundus.Types,
  Mundus.Diagnostics.StopWatch,
  Mundus.FrameBuffer;

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
    procedure SetResolutionX(const Value: Integer);
    procedure SetResolutionY(const Value: Integer);
    function GetFPS: Integer;
    function GetRenderFence: THandle;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create;
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
  Mundus.Shader;

{ TRenderWorker }

constructor TRenderWorker.Create;
begin
  inherited Create(True);
  FDone := TEvent.Create(nil, False, True, '');
  FStart := TEvent.Create(nil, False, False, '');
  FWatch := TStopWatch.Create();
end;

destructor TRenderWorker.Destroy;
begin
  inherited;
  FStart.Free;
  FDone.Free;
  FWatch.Free;
end;

procedure TRenderWorker.Execute;
var
  LCall: PDrawCall;
  LTriangle: PTriangle;
  i, k: Integer;
  LVertexA, LVertexB, LVertexC: TFloat4;
  LRasterizer: TRasterizer;
  LRenderTarget: Pointer;
  LFirstDepth, LFirstLowDepth: System.PSingle;
  LMinY, LMaxY: Integer;
begin
  NameThreadForDebugging('RenderWorker');
  while not Terminated do
  begin
    FStart.WaitFor();
    FWatch.Start;
    if Assigned(FDrawCalls) then
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
        for k := 0 to Pred(LCall.TriangleCount) do
        begin
          LTriangle := @LCall.Triangles[k];
          LVertexA := LCall.Vertices[LTriangle.VertexA];
          LVertexB := LCall.Vertices[LTriangle.VertexB];
          LVertexC := LCall.Vertices[LTriangle.VertexC];

          //denormalize vectors to screenpos
          LVertexA.Elements[0] := (1-LVertexA.Elements[0]) * FHalfResolutionX;//half screen size
          LVertexA.Elements[1] := (1-LVertexA.Elements[1]) * FHalfResolutionY;

          LVertexB.Elements[0] := (1-LVertexB.Elements[0]) * FHalfResolutionX;
          LVertexB.Elements[1] := (1-LVertexB.Elements[1]) * FHalfResolutionY;

          LVertexC.Elements[0] := (1-LVertexC.Elements[0]) * FHalfResolutionX;
          LVertexC.Elements[1] := (1-LVertexC.Elements[1]) * FHalfResolutionY;

          //check if triangle overlaps with workers render area. Skip if not intersecting
          if ((LVertexA.Y > LMaxY) and (LVertexB.Y > LMaxY) and (LVertexC.Y > LMaxY))
            or ((LVertexA.Y < LMinY) and (LVertexB.Y < LMinY) and (LVertexC.Y < LMinY))
          then
            Continue;

          LRasterizer(
            FMaxResolutionX, FMaxResolutionY,
            LVertexA, LVertexB, LVertexC,
            LCall.Attributes[LTriangle.VertexA],
            LCall.Attributes[LTriangle.VertexB],
            LCall.Attributes[LTriangle.VertexC],
            @LCall.ConstantValues[0],
            LRenderTarget,
            LFirstDepth,
            LFirstLowDepth,
            FBlockOffset, FBlockSteps, FBlockEnd);
        end;
      end;
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
