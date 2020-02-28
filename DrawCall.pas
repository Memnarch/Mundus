unit DrawCall;

interface

uses
  Generics.Collections,
  Math3D,
  BaseMesh;

type
  TDrawCall = class
  private
    FVertices: TObjectList<TVectorClass4D>;
    FTriangles: TObjectList<TTriangleClass>;
  public
    constructor Create;
    destructor Destroy; override;
    property Vertices: TObjectList<TVectorClass4D> read FVertices write FVertices;
    property Triangles: TObjectList<TTriangleClass> read FTriangles;
  end;

implementation

{ TDrawCall }

constructor TDrawCall.Create;
begin
  inherited;
  FVertices := TObjectList<TVectorClass4D>.Create();
  FTriangles := TObjectList<TTriangleClass>.Create();
end;

destructor TDrawCall.Destroy;
begin
  FVertices.Free;
  FTriangles.Free;
  inherited;
end;

end.
