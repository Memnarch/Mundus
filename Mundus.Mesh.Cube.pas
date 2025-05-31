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
    constructor Create(); reintroduce;
  end;

implementation

uses
  Mundus.Types;

{ TCube }

constructor TCube.Create;
var
  LVertices: array[0..7] of TVector;
  LUV: TArray<TFloat2>;
begin
  inherited Create();
  LVertices[0] := Vector(-32, -32, -32);
  LVertices[1] := Vector(-32, 32, -32);
  LVertices[2] := Vector(32, 32, -32);
  LVertices[3] := Vector(32, -32, -32);
  LVertices[4] := Vector(-32, -32, 32);
  LVertices[5] := Vector(-32, 32, 32);
  LVertices[6] := Vector(32, 32, 32);
  LVertices[7] := Vector(32, -32, 32);
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
// add uv
  //UV cooridnates
  SetLength(FUVs, 1);
  SetLength(FUVs[0], Length(FVertexList));
  LUV := FUVs[0];
  LUV[0] := TFloat2.Create(1, 1);
  LUV[1] := TFloat2.Create(1, 0);
  LUV[2] := TFloat2.Create(0, 0);

  LUV[3] := TFloat2.Create(0, 0);
  LUV[4] := TFloat2.Create(0, 1);
  LUV[5] := TFloat2.Create(1, 1);

  LUV[6] := TFloat2.Create(1, 1);
  LUV[7] := TFloat2.Create(1, 0);
  LUV[8] := TFloat2.Create(0, 0);

  LUV[9] := TFloat2.Create(0, 0);
  LUV[10] := TFloat2.Create(0, 1);
  LUV[11] := TFloat2.Create(1, 1);

  LUV[12] := TFloat2.Create(1, 1);
  LUV[13] := TFloat2.Create(0, 0);
  LUV[14] := TFloat2.Create(0, 1);

  LUV[15] := TFloat2.Create(1, 0);
  LUV[16] := TFloat2.Create(0, 0);
  LUV[17] := TFloat2.Create(1, 1);

  LUV[18] := TFloat2.Create(1, 0);
  LUV[19] := TFloat2.Create(0, 1);
  LUV[20] := TFloat2.Create(1, 1);

  LUV[21] := TFloat2.Create(1, 0);
  LUV[22] := TFloat2.Create(0, 0);
  LUV[23] := TFloat2.Create(0, 1);

  LUV[24] := TFloat2.Create(0, 1);
  LUV[25] := TFloat2.Create(1, 1);
  LUV[26] := TFloat2.Create(1, 0);

  LUV[27] := TFloat2.Create(0, 1);
  LUV[28] := TFloat2.Create(1, 0);
  LUV[29] := TFloat2.Create(0, 0);

  LUV[30] := TFloat2.Create(1, 1);
  LUV[31] := TFloat2.Create(1, 0);
  LUV[32] := TFloat2.Create(0, 0);

  LUV[33] := TFloat2.Create(0, 1);
  LUV[34] := TFloat2.Create(1, 1);
  LUV[35] := TFloat2.Create(0, 0);

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
  FPosition.Z := 200;
end;

end.
