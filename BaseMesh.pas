unit BaseMesh;

interface

uses
  Types, Classes, SysUtils, Generics.Collections, Math3d, RenderTypes, Shader;

type
  TBaseMesh = class
  private
    FShader: TShaderClass;
  protected
    FVertexList: TObjectList<TVectorClass>;
    FTriangleList: TObjectList<TTriangleClass>;
    FRotation: TFloat3;
    FPosition: TFloat3;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure AddVertice(AVertice: TVectorClass);
    property Triangles: TObjectList<TTriangleClass> read FTriangleList;
    property Vertices: TObjectList<TVectorClass> read FVertexList;
    property Position: TFloat3 read FPosition write FPosition;
    property Rotation: TFloat3 read FRotation write Frotation;
    property Shader: TShaderClass read FShader write FShader;
  end;

implementation

{ TBaseMesh }

procedure TBaseMesh.AddVertice(AVertice: TVectorClass);
begin
  FVertexList.Add(AVertice);
end;

constructor TBaseMesh.Create;
begin
  inherited;
  FVertexList := TObjectList<TVectorClass>.Create();
  FTriangleList := TObjectList<TTriangleClass>.Create();
end;

destructor TBaseMesh.Destroy;
begin
  FVertexList.Free();
  FTriangleList.Free();
  inherited;
end;

end.
