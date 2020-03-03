unit DrawCall;

interface

uses
  Generics.Collections,
  Math3D,
  BaseMesh;

type
//  TFloat4 = TVector4D;

  TDrawCall = class
  private
    FVertices: TArray<TFloat4>;
    FTriangles: TArray<TTriangle>;
  public
    procedure AddVertex(const AVertex: TVectorClass4D);
    procedure AddTriangle(const ATriangle: TTriangleClass);
    property Vertices: TArray<TFloat4> read FVertices write FVertices;
    property Triangles: TArray<TTriangle> read FTriangles;
  end;

implementation

{ TDrawCall }

{ TDrawCall }

procedure TDrawCall.AddTriangle(const ATriangle: TTriangleClass);
var
  LTriangle: PTriangle;
begin
  SetLength(FTriangles, Length(FTriangles)+1);
  LTriangle := @FTriangles[High(FTriangles)];
  LTriangle.VertexA := ATriangle.VertexA;
  LTriangle.VertexB := ATriangle.VertexB;
  LTriangle.VertexC := ATriangle.VertexC;
  LTriangle.UVA := ATriangle.UVA;
  LTriangle.UVB := ATriangle.UVB;
  LTriangle.UVC := ATriangle.UVC;
end;

procedure TDrawCall.AddVertex(const AVertex: TVectorClass4D);
var
  LVertex: PVector4D;
begin
  SetLength(FVertices, Length(FVertices)+1);
  LVertex := @FVertices[High(FVertices)];
  LVertex.X := AVertex.X;
  LVertex.Y := AVertex.Y;
  LVertex.Z := AVertex.Z;
  LVertex.W := AVertex.Element[3];
end;

end.
