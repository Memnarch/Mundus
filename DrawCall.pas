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
    procedure AddVertex(const AVertex: TFloat4);
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

procedure TDrawCall.AddVertex(const AVertex: TFloat4);
begin
  SetLength(FVertices, Length(FVertices)+1);
  FVertices[High(FVertices)] := AVertex;
end;

end.
