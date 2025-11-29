unit Mundus.Mesh;

interface

uses
  Types,
  Classes,
  SysUtils,
  Generics.Collections,
  Mundus.Math,
  Mundus.Types,
  Mundus.Shader,
  Mundus.Material;

type
  TMesh = class;

  TTextureReference = record
    Name: string;
    FileName: string;
  end;

  TMesh = class
  private
    FShader: PShaderInfo;
    FMaterial: TMaterial;
    FNormals: TArray<TVector>;
    FTextures: TArray<TTextureReference>;
    FName: string;
  protected
    FVertexList: TArray<TVector>;
    FTriangles: TArray<TTriangle>;
    FUVs: TArray<TArray<TUV>>;
    FRotation: TFloat3;
    FPosition: TFloat3;
  public
    function AddVertice(const AVertice: TVector): Integer;
    function AddTriangle(const ATriangle: TTriangle): Integer;
    function AddUV(const AUV: TUV; AUVSet: Integer = 0): Integer;
    function AddNormal(const ANormal: TVector): Integer;
    function AddTextureReference(const AReference: TTextureReference): Integer;
    property Triangles: TArray<TTriangle> read FTriangles;
    property Vertices: TArray<TVector> read FVertexList;
    property Normals: TArray<TVector> read FNormals;
    property UVs: TArray<TArray<TUV>> read FUVs;
    property Position: TFloat3 read FPosition write FPosition;
    property Rotation: TFloat3 read FRotation write Frotation;
    property Shader: PShaderInfo read FShader write FShader;
    property Material: TMaterial read FMaterial write FMaterial;
    property Textures: TArray<TTextureReference> read FTextures;
    property Name: string read FName write FName;
  end;

  TMeshGroup = class
  private
    FMeshes: TObjectList<TMesh>;
  public
    constructor Create;
    destructor Destroy; override;
    property Meshes: TObjectList<TMesh> read FMeshes;
  end;

implementation

function TMesh.AddNormal(const ANormal: TVector): Integer;
begin
  Result := Length(FNormals);
  SetLength(FNormals, Result + 1);
  FNormals[Result] := ANormal;
end;

function TMesh.AddTextureReference(const AReference: TTextureReference): Integer;
begin
  Result := Length(FTextures);
  SetLength(FTextures, Result + 1);
  FTextures[Result] := AReference;
end;

{ TBaseMesh }

function TMesh.AddTriangle(const ATriangle: TTriangle): Integer;
begin
  Result := Length(FTriangles);
  SetLength(FTriangles, Length(FTriangles)+1);
  FTriangles[High(FTriangles)] := ATriangle;
end;

function TMesh.AddUV(const AUV: TUV; AUVSet: Integer = 0): Integer;
begin
  if High(FUVs) < AUVSet then
    SetLength(FUVs, AUVSet+1);

  Result := Length(FUVs[AUVSet]);
  SetLength(FUVs[AUVSet], Result+1);
  FUVs[AUVSet, Result] := AUV;
end;

function TMesh.AddVertice(const AVertice: TVector): Integer;
begin
  Result := Length(FVertexList);
  SetLength(FVertexList, Length(FVertexList)+1);
  FVertexList[Result] := AVertice;
end;

{ TMeshGroup }

constructor TMeshGroup.Create;
begin
  inherited;
  FMeshes := TObjectList<TMesh>.Create();
end;

destructor TMeshGroup.Destroy;
begin
  FMeshes.Free;
  inherited;
end;

end.
