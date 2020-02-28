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
    FDrawCalls: TObjectList<TDrawCall>;
    FDone: TEvent;
    FStart: TEvent;
    FBlockSteps: Integer;
    FBlockOffset: Integer;
    FShader: TTextureShader;
    FVertexA: TVectorClass4D;
    FVertexB: TVectorClass4D;
    FVertexC: TVectorClass4D;
    FNormal: TVectorClass4D;
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
    property DrawCalls: TObjectList<TDrawCall> read FDrawCalls write FDrawCalls;
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
  FVertexA := TVectorClass4D.Create();
  FVertexB := TVectorClass4D.Create();
  FVertexC := TVectorClass4D.Create();
  FNormal := TVectorClass4D.Create();
end;

destructor TRenderWorker.Destroy;
begin
  inherited;
  FStart.Free;
  FDone.Free;
  FShader.Free;
  FVertexA.Free;
  FVertexB.Free;
  FVertexC.Free;
  FNormal.Free;
end;

procedure TRenderWorker.Execute;
var
  LCall: TDrawCall;
  LTriangle: TTriangleClass;
begin
  while not Terminated do
  begin
    FStart.WaitFor();
    for LCall in FDrawCalls do
    begin
      for LTriangle in LCall.Triangles do
      begin
        FVertexA.CopyFromVector4D(LCall.Vertices[LTriangle.VertexA]);
        FVertexB.CopyFromVector4D(LCall.Vertices[LTriangle.VertexB]);
        FVertexC.CopyFromVector4D(LCall.Vertices[LTriangle.VertexC]);


        FNormal.CalculateSurfaceNormal(FVertexA, FVertexB, FVertexC);
        if (FNormal.Z < 0)then
        begin
          //denormalize vectors to screenpos
          FVertexA.Element[0] := (1-FVertexA.Element[0]) * FHalfResolutionX;//half screen size
          FVertexA.Element[1] := (1-FVertexA.Element[1]) * FHalfResolutionY;
          FVertexB.Element[0] := (1-FVertexB.Element[0]) * FHalfResolutionX;
          FVertexB.Element[1] := (1-FVertexB.Element[1]) * FHalfResolutionY;
          FVertexC.Element[0] := (1-FVertexC.Element[0]) * FHalfResolutionX;
          FVertexC.Element[1] := (1-FVertexC.Element[1]) * FHalfResolutionY;

          FShader.InitTriangle(FVertexA, FVertexB, FVertexC);
          TTextureShader(FShader).InitUV(LTriangle.UVA, LTriangle.UVB, LTriangle.UVC);

          RasterizeTriangle(FMaxResolutionX, FMaxResolutionY, FVertexA, FVertexB,
            FVertexC, FShader, FBlockOffset, FBlockSteps);

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
