unit Cube;

interface
uses
  Classes, Types, SysUtils, BaseMesh;

type
  TCube = class(TBaseMesh)
  private

  public
    constructor Create(); reintroduce;
  end;

implementation

{ TCube }

constructor TCube.Create;
var
  LTriangle: TTriangleClass;
begin
  inherited Create();
  //front vertices
   
  AddVertice(TVectorClass.Create(-32, -32, -32));
  AddVertice(TVectorClass.Create(-32, 32, -32));
  AddVertice(TVectorClass.Create(32, 32, -32));
  AddVertice(TVectorClass.Create(32, -32, -32));
//  // backvertices
  AddVertice(TVectorClass.Create(-32, -32, 32));
  AddVertice(TVectorClass.Create(-32, 32, 32));
  AddVertice(TVectorClass.Create(32, 32, 32));
  AddVertice(TVectorClass.Create(32, -32, 32));

  //FrontFace
  LTriangle := TTriangleClass.Create(0, 1, 2);
  LTriangle.SetUV(UV(1, 1), UV(1, 0), UV(0, 0));
  Triangles.Add(LTriangle);

  LTriangle := TTriangleClass.Create(2, 3, 0);//2, 3, 0);
//  LTriangle.SetUV(UV(0, 1), UV(1, 1), UV(0, 0));
  LTriangle.SetUV(UV(0, 0), UV(0, 1), UV(1, 1));
  Triangles.Add(LTriangle);

  //LeftSide
  LTriangle := TTriangleClass.Create(3, 2, 6);
  LTriangle.SetUV(UV(1, 1), UV(1, 0), UV(0, 0));
  Triangles.Add(LTriangle);
  LTriangle := TTriangleClass.Create(6, 7, 3);
  LTriangle.SetUV(UV(0, 0), UV(0, 1), UV(1, 1));
  Triangles.Add(LTriangle);

  //BackSide
  LTriangle := TTriangleClass.Create(7, 5, 4);
  LTriangle.SetUV(UV(1, 1),  UV(0, 0), UV(0, 1));
  Triangles.Add(LTriangle);
  LTriangle := TTriangleClass.Create(6, 5, 7);
  LTriangle.SetUV(UV(1, 0),  UV(0, 0), UV(1, 1));
  Triangles.Add(LTriangle);
  //RightSide;
  LTriangle := TTriangleClass.Create(5, 0, 4);
  LTriangle.SetUV(UV(1, 0),  UV(0, 1), UV(1, 1));
  Triangles.Add(LTriangle);
  LTriangle := TTriangleClass.Create(5, 1, 0);
  LTriangle.SetUV(UV(1, 0),  UV(0, 0), UV(0, 1));
  Triangles.Add(LTriangle);

  //TopSide
  LTriangle := TTriangleClass.Create(2, 1, 5);
  LTriangle.SetUV(UV(0, 1),  UV(1, 1), UV(1, 0));
  Triangles.Add(LTriangle);
  LTriangle := TTriangleClass.Create(2, 5, 6);
  LTriangle.SetUV(UV(0, 1), UV(1, 0), UV(0, 0));
  Triangles.Add(LTriangle);
  //BottomSide
  LTriangle := TTriangleClass.Create(4, 0, 3);
  LTriangle.SetUV(UV(1, 1),  UV(1, 0), UV(0, 0));
  Triangles.Add(LTriangle);
  LTriangle := TTriangleClass.Create(7, 4, 3);
  LTriangle.SetUV(UV(0, 1), UV(1, 1), UV(0, 0));
  Triangles.Add(LTriangle);
  Position := Vector(0, 0, 200);
end;

end.
