unit Mundus.Mesh;

interface

uses
  Types,
  Classes,
  SysUtils,
  Generics.Collections,
  Mundus.Math,
  Mundus.Types,
  Mundus.Shader;

type
  TMesh = class
  private
    FShader: TShaderClass;
  protected
    FVertexList: TArray<TVector>;
    FTriangleList: TObjectList<TTriangleClass>;
    FUV: TArray<TFloat2>;
    FRotation: TFloat3;
    FPosition: TFloat3;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure AddVertice(AVertice: TVector);
    property Triangles: TObjectList<TTriangleClass> read FTriangleList;
    property Vertices: TArray<TVector> read FVertexList;
    property UV: TArray<TFloat2> read FUV;
    property Position: TFloat3 read FPosition write FPosition;
    property Rotation: TFloat3 read FRotation write Frotation;
    property Shader: TShaderClass read FShader write FShader;
  end;

implementation

{ TBaseMesh }

procedure TMesh.AddVertice(AVertice: TVector);
begin
  SetLength(FVertexList, Length(FVertexList)+1);
  FVertexList[High(FVertexList)] := AVertice;
end;

constructor TMesh.Create;
begin
  inherited;
  FTriangleList := TObjectList<TTriangleClass>.Create();
end;

destructor TMesh.Destroy;
begin
  FTriangleList.Free();
  inherited;
end;

end.
