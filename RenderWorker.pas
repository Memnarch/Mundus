unit RenderWorker;

interface

uses
  Classes,
  SyncObjs,
  Generics.Collections,
  DrawCall,
  TextureShader,
  Math3D,
  StopWatch;

type
  TRenderWorker = class(TThread)
  private
    FDrawCalls: TDrawCalls;
    FDone: TEvent;
    FStart: TEvent;
    FBlockSteps: Integer;
    FBlockOffset: Integer;
    FShader: TTextureShader;
    FResolutionX: Integer;
    FResolutionY: Integer;
    FMaxResolutionX: Integer;
    FMaxResolutionY: Integer;
    FHalfResolutionX: Integer;
    FHalfResolutionY: Integer;
    FWatch: TStopWatch;
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
    property Shader: TTextureShader read FShader;
    property ResolutionX: Integer read FResolutionX write SetResolutionX;
    property ResolutionY: Integer read FResolutionY write SetResolutionY;
    property FPS: Integer read GetFPS;
    property RenderFence: THandle read GetRenderFence;
  end;

implementation

uses
  Rasterizer,
  BaseMesh;

{ TRenderWorker }

constructor TRenderWorker.Create;
begin
  inherited Create(True);
  FDone := TEvent.Create(nil, False, True, '');
  FStart := TEvent.Create(nil, False, False, '');
  FShader := TTextureShader.Create();
  FWatch := TStopWatch.Create();
end;

destructor TRenderWorker.Destroy;
begin
  inherited;
  FStart.Free;
  FDone.Free;
  FShader.Free;
  FWatch.Free;
end;

procedure TRenderWorker.Execute;
var
  LCall: PDrawCall;
  LTriangle: TTriangle;
  i, k: Integer;
  LVertexA, LVertexB, LVertexC, LNormal: TFloat4;
begin
  while not Terminated do
  begin
    FStart.WaitFor();
    FWatch.Start;
    for i := 0 to Pred(FDrawCalls.Count) do
    begin
      LCall := FDrawCalls[i];
      for k := 0 to Pred(LCall.TriangleCount) do
      begin
        LTriangle := LCall.Triangles[k];
        LVertexA := LCall.Vertices[LTriangle.VertexA];
        LVertexB := LCall.Vertices[LTriangle.VertexB];
        LVertexC := LCall.Vertices[LTriangle.VertexC];
        LNormal.CalculateSurfaceNormal(LVertexA, LVertexB, LVertexC);
        if (LNormal.Z < 0)then
        begin
          //denormalize vectors to screenpos
          LVertexA.Element[0] := (1-LVertexA.Element[0]) * FHalfResolutionX;//half screen size
          LVertexA.Element[1] := (1-LVertexA.Element[1]) * FHalfResolutionY;
          LVertexB.Element[0] := (1-LVertexB.Element[0]) * FHalfResolutionX;
          LVertexB.Element[1] := (1-LVertexB.Element[1]) * FHalfResolutionY;
          LVertexC.Element[0] := (1-LVertexC.Element[0]) * FHalfResolutionX;
          LVertexC.Element[1] := (1-LVertexC.Element[1]) * FHalfResolutionY;

          FShader.InitTriangle(LVertexA, LVertexB, LVertexC);
          TTextureShader(FShader).InitUV(LTriangle.UVA, LTriangle.UVB, LTriangle.UVC);

          RasterizeTriangle(FMaxResolutionX, FMaxResolutionY, LVertexA, LVertexB,
            LVertexC, FShader, FBlockOffset, FBlockSteps);

//          FPolyCount := FPolyCount + 1;
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
