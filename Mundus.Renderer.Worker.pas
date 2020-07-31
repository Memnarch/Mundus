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
  Mundus.Diagnostics.StopWatch;

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
    FPixelBuffer: TBitmap;
    FDepthBuffer: PDepthsBuffer;
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
    property ResolutionX: Integer read FResolutionX write SetResolutionX;
    property ResolutionY: Integer read FResolutionY write SetResolutionY;
    property PixelBuffer: TBitmap read FPixelBuffer write FPixelBuffer;
    property DepthBuffer: PDepthsBuffer read FDepthBuffer write FDepthBuffer;
    property FPS: Integer read GetFPS;
    property RenderFence: THandle read GetRenderFence;
  end;

implementation

uses
  Windows,
  Mundus.Rasterizer,
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
  LTriangle: TTriangle;
  i, k: Integer;
  LVertexA, LVertexB, LVertexC, LNormal: TFloat4;
  LShader: TShader;
  LRasterizer: TRasterizer;
  LRenderTarget: Pointer;
  LFirstDepth: System.PSingle;
begin
  while not Terminated do
  begin
    FStart.WaitFor();
    FWatch.Start;
    if Assigned(FDrawCalls) then
    begin
      LRenderTarget := FPixelBuffer.ScanLine[0];
      LFirstDepth := @FDepthBuffer^[0];
      Inc(LFirstDepth, (FPixelBuffer.Height-1)*FPixelBuffer.Width);
      for i := 0 to Pred(FDrawCalls.Count) do
      begin
        LCall := FDrawCalls[i];
        LShader := LCall.Shader.Create();
        LShader.PixelBuffer := FPixelBuffer;
        LShader.BindBuffer(@LCall.Values);
        LRasterizer := LCall.Shader.GetRasterizer();
        for k := 0 to Pred(LCall.TriangleCount) do
        begin
          LTriangle := LCall.Triangles[k];
          LVertexA := LCall.Vertices[LTriangle.VertexA];
          LVertexB := LCall.Vertices[LTriangle.VertexB];
          LVertexC := LCall.Vertices[LTriangle.VertexC];

          //denormalize vectors to screenpos
          LVertexA.Element[0] := (1-LVertexA.Element[0]) * FHalfResolutionX;//half screen size
          LVertexA.Element[1] := (1-LVertexA.Element[1]) * FHalfResolutionY;
          LVertexB.Element[0] := (1-LVertexB.Element[0]) * FHalfResolutionX;
          LVertexB.Element[1] := (1-LVertexB.Element[1]) * FHalfResolutionY;
          LVertexC.Element[0] := (1-LVertexC.Element[0]) * FHalfResolutionX;
          LVertexC.Element[1] := (1-LVertexC.Element[1]) * FHalfResolutionY;

          LRasterizer(
            FMaxResolutionX, FMaxResolutionY,
            LVertexA, LVertexB, LVertexC,
            LCall.Attributes[LTriangle.VertexA],
            LCall.Attributes[LTriangle.VertexB],
            LCall.Attributes[LTriangle.VertexC],
            LShader,
            LRenderTarget,
            LFirstDepth,
            FBlockOffset, FBlockSteps);
        end;
        LShader.Free;
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
    Result := 1000;
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
