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
   
  Vertices.Add(TVectorClass.Create(-32, -32, -32));
  Vertices.Add(TVectorClass.Create(-32, 32, -32));
  Vertices.Add(TVectorClass.Create(32, 32, -32));
  Vertices.Add(TVectorClass.Create(32, -32, -32));
//  // backvertices
  Vertices.Add(TVectorClass.Create(-32, -32, 32));
  Vertices.Add(TVectorClass.Create(-32, 32, 32));
  Vertices.Add(TVectorClass.Create(32, 32, 32));
  Vertices.Add(TVectorClass.Create(32, -32, 32));

  //FrontFace
  LTriangle := TTriangleClass.Create(0, 1, 2);
  LTriangle.SetUV(UV(1, 1), UV(1, 0), UV(0, 0));
  Triangles.Add(LTriangle);

  LTriangle := TTriangleClass.Create(2, 3, 0);
  LTriangle.SetUV(UV(0, 0), UV(0, 1), UV(1, 1));
  Triangles.Add(LTriangle);

  //LeftSide
  Triangles.Add(TTriangleClass.Create(3, 2, 6));
  Triangles.Add(TTriangleClass.Create(6, 7, 3));

  //BackSide
  Triangles.Add(TTriangleClass.Create(7, 5, 4));
  Triangles.Add(TTriangleClass.Create(6, 5, 7));
  //RightSide;
  Triangles.Add(TTriangleClass.Create(5, 0, 4));
  Triangles.Add(TTriangleClass.Create(5, 1, 0));

  //TopSide
  Triangles.Add(TTriangleClass.Create(2, 1, 5));
  Triangles.Add(TTriangleClass.Create(2, 5, 6));
  //BottomSide
  Triangles.Add(TTriangleClass.Create(4, 0, 3));
  Triangles.Add(TTriangleClass.Create(7, 4, 3));
  Position := Vector(0, 0, 200);
end;

end.
