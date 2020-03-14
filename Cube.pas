unit Cube;

interface
uses
  Classes, Types, SysUtils, BaseMesh, Math3D;

type
  TCube = class(TBaseMesh)
  private

  public
    constructor Create(); reintroduce;
  end;

implementation

uses
  RenderTypes;

{ TCube }

constructor TCube.Create;
var
  LTriangle: TTriangleClass;
  LVertices: array[0..7] of TVector;
begin
  inherited Create();
  LVertices[0] := TVector.Create(-32, -32, -32);
  LVertices[1] := TVector.Create(-32, 32, -32);
  LVertices[2] := TVector.Create(32, 32, -32);
  LVertices[3] := TVector.Create(32, -32, -32);
  LVertices[4] := TVector.Create(-32, -32, 32);
  LVertices[5] := TVector.Create(-32, 32, 32);
  LVertices[6] := TVector.Create(32, 32, 32);
  LVertices[7] := TVector.Create(32, -32, 32);
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
  SetLength(FUV, Length(FVertexList));
  FUV[0] := TFloat2.Create(1, 1);
  FUV[1] := TFloat2.Create(1, 0);
  FUV[2] := TFloat2.Create(0, 0);

  FUV[3] := TFloat2.Create(0, 0);
  FUV[4] := TFloat2.Create(0, 1);
  FUV[5] := TFloat2.Create(1, 1);

  FUV[6] := TFloat2.Create(1, 1);
  FUV[7] := TFloat2.Create(1, 0);
  FUV[8] := TFloat2.Create(0, 0);

  FUV[9] := TFloat2.Create(0, 0);
  FUV[10] := TFloat2.Create(0, 1);
  FUV[11] := TFloat2.Create(1, 1);

  FUV[12] := TFloat2.Create(1, 1);
  FUV[13] := TFloat2.Create(0, 0);
  FUV[14] := TFloat2.Create(0, 1);

  FUV[15] := TFloat2.Create(1, 0);
  FUV[16] := TFloat2.Create(0, 0);
  FUV[17] := TFloat2.Create(1, 1);

  FUV[18] := TFloat2.Create(1, 0);
  FUV[19] := TFloat2.Create(0, 1);
  FUV[20] := TFloat2.Create(1, 1);

  FUV[21] := TFloat2.Create(1, 0);
  FUV[22] := TFloat2.Create(0, 0);
  FUV[23] := TFloat2.Create(0, 1);

  FUV[24] := TFloat2.Create(0, 1);
  FUV[25] := TFloat2.Create(1, 1);
  FUV[26] := TFloat2.Create(1, 0);

  FUV[27] := TFloat2.Create(0, 1);
  FUV[28] := TFloat2.Create(1, 0);
  FUV[29] := TFloat2.Create(0, 0);

  FUV[30] := TFloat2.Create(1, 1);
  FUV[31] := TFloat2.Create(1, 0);
  FUV[32] := TFloat2.Create(0, 0);

  FUV[33] := TFloat2.Create(0, 1);
  FUV[34] := TFloat2.Create(1, 1);
  FUV[35] := TFloat2.Create(0, 0);

  //FrontFace
  LTriangle := TTriangleClass.Create(0, 1, 2);
//  LTriangle.SetUV(UV(1, 1), UV(1, 0), UV(0, 0));
  Triangles.Add(LTriangle);

  LTriangle := TTriangleClass.Create(3, 4, 5);//2, 3, 0);
//  LTriangle.SetUV(UV(0, 0), UV(0, 1), UV(1, 1));
  Triangles.Add(LTriangle);

  //LeftSide
  LTriangle := TTriangleClass.Create(6, 7, 8);
//  LTriangle.SetUV(UV(1, 1), UV(1, 0), UV(0, 0));
  Triangles.Add(LTriangle);
  LTriangle := TTriangleClass.Create(9, 10, 11);
//  LTriangle.SetUV(UV(0, 0), UV(0, 1), UV(1, 1));
  Triangles.Add(LTriangle);

  //BackSide
  LTriangle := TTriangleClass.Create(12, 13, 14);
//  LTriangle.SetUV(UV(1, 1),  UV(0, 0), UV(0, 1));
  Triangles.Add(LTriangle);
  LTriangle := TTriangleClass.Create(15, 16, 17);
//  LTriangle.SetUV(UV(1, 0),  UV(0, 0), UV(1, 1));
  Triangles.Add(LTriangle);
  //RightSide;
  LTriangle := TTriangleClass.Create(18, 19, 20);
//  LTriangle.SetUV(UV(1, 0),  UV(0, 1), UV(1, 1));
  Triangles.Add(LTriangle);
  LTriangle := TTriangleClass.Create(21, 22, 23);
//  LTriangle.SetUV(UV(1, 0),  UV(0, 0), UV(0, 1));
  Triangles.Add(LTriangle);

  //TopSide
  LTriangle := TTriangleClass.Create(24, 25, 26);
//  LTriangle.SetUV(UV(0, 1),  UV(1, 1), UV(1, 0));
  Triangles.Add(LTriangle);
  LTriangle := TTriangleClass.Create(27, 28, 29);
//  LTriangle.SetUV(UV(0, 1), UV(1, 0), UV(0, 0));
  Triangles.Add(LTriangle);
  //BottomSide
  LTriangle := TTriangleClass.Create(30, 31, 32);
//  LTriangle.SetUV(UV(1, 1),  UV(1, 0), UV(0, 0));
  Triangles.Add(LTriangle);
  LTriangle := TTriangleClass.Create(33, 34, 35);
//  LTriangle.SetUV(UV(0, 1), UV(1, 1), UV(0, 0));
  Triangles.Add(LTriangle);
  FPosition.Z := 200;
end;

end.
