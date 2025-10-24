unit Mundus.Mesh.Cube;

interface
uses
  Classes,
  Types,
  SysUtils,
  Mundus.Mesh,
  Mundus.Math;

type
  TCube = class(TMesh)
  private

  public
    constructor Create(const ASizeX, ASizeY, ASizeZ: Single); reintroduce;
  end;

implementation

uses
  Mundus.Types;

{ TCube }

constructor TCube.Create(const ASizeX, ASizeY, ASizeZ: Single);
var
  LVertices: array[0..7] of TVector;
  LUV: TArray<TUV>;
  LMin, LMax: TVector;
  i: Integer;
begin
  inherited Create();
  LMin := Vector(-ASizeX / 2, -ASizeY / 2, -ASizeZ / 2);
  LMax := Vector(ASizeX / 2, ASizeY / 2, ASizeZ / 2);
  LVertices[0] := LMin;
  LVertices[1] := Vector(LMin.X, LMax.Y, LMin.Z);
  LVertices[2] := Vector(LMax.X, LMax.Y, LMin.Z);
  LVertices[3] := Vector(LMax.X, LMin.Y, LMin.Z);
  LVertices[4] := Vector(LMin.X, LMin.Y, LMax.Z);
  LVertices[5] := Vector(LMin.X, LMax.Y, LMax.Z);
  LVertices[6] := LMax;
  LVertices[7] := Vector(LMax.X, LMin.Y, LMax.Z);
  //add vertices, simply one vertex per triangle corner to simplify uv mapping
  AddVertice(LVertices[0]);
  AddVertice(LVertices[1]);
  AddVertice(LVertices[2]);
  AddVertice(LVertices[2]);
  AddVertice(LVertices[3]);
  AddVertice(LVertices[0]);
  AddVertice(LVertices[3]);
  AddVertice(LVertices[2]);
  AddVertice(LVertices[6]);
  AddVertice(LVertices[6]);
  AddVertice(LVertices[7]);
  AddVertice(LVertices[3]);
  AddVertice(LVertices[7]);
  AddVertice(LVertices[5]);
  AddVertice(LVertices[4]);
  AddVertice(LVertices[6]);
  AddVertice(LVertices[5]);
  AddVertice(LVertices[7]);
  AddVertice(LVertices[5]);
  AddVertice(LVertices[0]);
  AddVertice(LVertices[4]);
  AddVertice(LVertices[5]);
  AddVertice(LVertices[1]);
  AddVertice(LVertices[0]);
  AddVertice(LVertices[2]);
  AddVertice(LVertices[1]);
  AddVertice(LVertices[5]);
  AddVertice(LVertices[2]);
  AddVertice(LVertices[5]);
  AddVertice(LVertices[6]);
  AddVertice(LVertices[4]);
  AddVertice(LVertices[0]);
  AddVertice(LVertices[3]);
  AddVertice(LVertices[7]);
  AddVertice(LVertices[4]);
  AddVertice(LVertices[3]);

  for i := 0 to High(Vertices) do
  begin
    AddNormal(Vertices[i].Normalized);
  end;
// add uv
  //UV cooridnates
  SetLength(FUVs, 1);
  SetLength(FUVs[0], Length(FVertexList));
  LUV := FUVs[0];
  LUV[0] := UV(1, 1);
  LUV[1] := UV(1, 0);
  LUV[2] := UV(0, 0);

  LUV[3] := UV(0, 0);
  LUV[4] := UV(0, 1);
  LUV[5] := UV(1, 1);

  LUV[6] := UV(1, 1);
  LUV[7] := UV(1, 0);
  LUV[8] := UV(0, 0);

  LUV[9] := UV(0, 0);
  LUV[10] := UV(0, 1);
  LUV[11] := UV(1, 1);

  LUV[12] := UV(1, 1);
  LUV[13] := UV(0, 0);
  LUV[14] := UV(0, 1);

  LUV[15] := UV(1, 0);
  LUV[16] := UV(0, 0);
  LUV[17] := UV(1, 1);

  LUV[18] := UV(1, 0);
  LUV[19] := UV(0, 1);
  LUV[20] := UV(1, 1);

  LUV[21] := UV(1, 0);
  LUV[22] := UV(0, 0);
  LUV[23] := UV(0, 1);

  LUV[24] := UV(0, 1);
  LUV[25] := UV(1, 1);
  LUV[26] := UV(1, 0);

  LUV[27] := UV(0, 1);
  LUV[28] := UV(1, 0);
  LUV[29] := UV(0, 0);

  LUV[30] := UV(1, 1);
  LUV[31] := UV(1, 0);
  LUV[32] := UV(0, 0);

  LUV[33] := UV(0, 1);
  LUV[34] := UV(1, 1);
  LUV[35] := UV(0, 0);

  //FrontFace
  AddTriangle(Triangle(0, 1, 2));
  AddTriangle(Triangle(3, 4, 5));

  //LeftSide
  AddTriangle(Triangle(6, 7, 8));
  AddTriangle(Triangle(9, 10, 11));

  //BackSide
  AddTriangle(Triangle(12, 13, 14));
  AddTriangle(Triangle(15, 16, 17));
  //RightSide;
  AddTriangle(Triangle(18, 19, 20));
  AddTriangle(Triangle(21, 22, 23));

  //TopSide
  AddTriangle(Triangle(24, 25, 26));
  AddTriangle(Triangle(27, 28, 29));
  //BottomSide
  AddTriangle(Triangle(30, 31, 32));
  AddTriangle(Triangle(33, 34, 35));
end;

end.
