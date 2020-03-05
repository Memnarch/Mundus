unit RenderWorker;

interface

uses
  Classes,
  SyncObjs,
  Generics.Collections,
  DrawCall,
  TextureShader,
  Math3D;

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
    procedure SetResolutionX(const Value: Integer);
    procedure SetResolutionY(const Value: Integer);
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartRender;
    procedure WaitForRender;
    property DrawCalls: TDrawCalls read FDrawCalls write FDrawCalls;
    property BlockSteps: Integer read FBlockSteps write FBlockSteps;
    property BlockOffset: Integer read FBlockOffset write FBlockOffset;
    property Shader: TTextureShader read FShader;
    property ResolutionX: Integer read FResolutionX write SetResolutionX;
    property ResolutionY: Integer read FResolutionY write SetResolutionY;
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
end;

destructor TRenderWorker.Destroy;
begin
  inherited;
  FStart.Free;
  FDone.Free;
  FShader.Free;
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
    FDone.SetEvent;
  end;
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

procedure TRenderWorker.WaitForRender;
begin
  FDone.WaitFor();
end;

end.
