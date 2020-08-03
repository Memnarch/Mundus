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

  TTriangleEnumerator = packed record
    FCurentTriangle: PTriangle;
    FCount: Integer;
  private
    function GetCurrent: PTriangle; inline;
  public
    function MoveNext: Boolean; inline;
    property Current: PTriangle read GetCurrent;
  end;

  //dummy class to have something as collectiontype
  TTriangles = class
  public
    function GetEnumerator: TTriangleEnumerator; //inline;
  end;

  TMesh = class
  private
    FShader: TShaderClass;
    FMaterial: TMaterial;
    function GetTriangles: TTriangles; inline;
  protected
    FVertexList: TArray<TVector>;
    FTriangles: TArray<TTriangle>;
    FUV: TArray<TFloat2>;
    FRotation: TFloat3;
    FPosition: TFloat3;
  public
    function AddVertice(const AVertice: TVector): Integer;
    function AddTriangle(const ATriangle: TTriangle): Integer;
    function AddUV(const AUV: TFloat2): Integer;
    property Triangles: TTriangles read GetTriangles;
    property Vertices: TArray<TVector> read FVertexList;
    property UV: TArray<TFloat2> read FUV;
    property Position: TFloat3 read FPosition write FPosition;
    property Rotation: TFloat3 read FRotation write Frotation;
    property Shader: TShaderClass read FShader write FShader;
    property Material: TMaterial read FMaterial write FMaterial;
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

{ TBaseMesh }

function TMesh.AddTriangle(const ATriangle: TTriangle): Integer;
begin
  Result := Length(FTriangles);
  SetLength(FTriangles, Length(FTriangles)+1);
  FTriangles[High(FTriangles)] := ATriangle;
end;

function TMesh.AddUV(const AUV: TFloat2): Integer;
begin
  Result := Length(FUV);
  SetLength(FUV, Length(FUV)+1);
  FUV[High(FUV)] := AUV;
end;

function TMesh.AddVertice(const AVertice: TVector): Integer;
begin
  Result := Length(FVertexList);
  SetLength(FVertexList, Length(FVertexList)+1);
  FVertexList[High(FVertexList)] := AVertice;
end;

function TMesh.GetTriangles: TTriangles;
begin
  Result := TTriangles(Self);
end;

{ TTriangleEnumerator }

function TTriangleEnumerator.GetCurrent: PTriangle;
begin
  Result := FCurentTriangle;
end;

function TTriangleEnumerator.MoveNext: Boolean;
begin
  Inc(FCurentTriangle);
  Dec(FCount);
  Result := FCount > -1;
end;

{ TTriangles }

function TTriangles.GetEnumerator: TTriangleEnumerator;
begin
  Result.FCount := Length(TMesh(Self).FTriangles);
  Result.FCurentTriangle := @TMesh(Self).FTriangles[0];
  Dec(Result.FCurentTriangle);
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
