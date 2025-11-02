unit Mundus.GeometryBuffer;

interface

uses
  Mundus.Math,
  Mundus.Shader,
  Mundus.ValueBuffer;

type
  TGeometryBuffer = record
  private
    FShader: PShaderInfo;
    FValues: TValueBuffer;
    FUniformValues: TValueBuffer;
    FVertices: TArray<TFloat3>;
    FVertexIndices: TArray<Int32>;
    procedure SetupValueBuffers;
  public
    class function Create: TGeometryBuffer; static;
    procedure BindShader(AShader: PShaderInfo);
    procedure BindVertices(const AVertices: TArray<TFloat3>);
    procedure BindIndexedVertices(const AVertices: TArray<TFloat3>; const AIndices: TArray<Int32>);
    property Vertices: TArray<TFloat3> read FVertices;
    property VertexIndices: TArray<Int32> read FVertexIndices;
    property Shader: PShaderInfo read FShader;
    property UniformValues: TValueBuffer read FUniformValues;
    property Values: TValueBuffer read FValues;
  end;

  PGeometryBuffer = ^TGeometryBuffer;

  TGeometryBuffers = record
  private
    FBuffers: TArray<TGeometryBuffer>;
    FCount: Integer;
    function GetGeometry(Index: Integer): PGeometryBuffer;
  public
    function Add: PGeometryBuffer; overload;
    function Add(const AGeometry: TGeometryBuffer): PGeometryBuffer; overload;
    procedure Clear;
    property Geometries[Index: Integer]: PGeometryBuffer read GetGeometry;
    property Count: Integer read FCount;
  end;

  PGeometryBuffers = ^TGeometryBuffers;

implementation

{ TGeometryBuffer }

procedure TGeometryBuffer.BindIndexedVertices(const AVertices: TArray<TFloat3>; const AIndices: TArray<Int32>);
begin
  FVertices := AVertices;
  FVertexIndices := AIndices;
  SetupValueBuffers;
end;

procedure TGeometryBuffer.BindShader(AShader: PShaderInfo);
begin
  FShader := AShader;
  SetupValueBuffers;
end;

procedure TGeometryBuffer.BindVertices(const AVertices: TArray<TFloat3>);
begin
  FVertices := AVertices;
  FVertexIndices := nil;
  SetupValueBuffers;
end;

class function TGeometryBuffer.Create: TGeometryBuffer;
begin
  Result := Default(TGeometryBuffer);
end;

procedure TGeometryBuffer.SetupValueBuffers;
begin
  if Assigned(FShader) and Assigned(FVertices) then
  begin
    FUniformValues.Initialize(FShader.ConstantBufferDescriptor, 1);
    FValues.Initialize(FShader.VertexBufferDescriptor, Length(FVertices));
  end;
end;

{ TGeometryBuffers }

function TGeometryBuffers.Add: PGeometryBuffer;
begin
  Inc(FCount);
  if Length(FBuffers) < FCount then
  begin
    SetLength(FBuffers, FCount);
  end;
  Result := @FBuffers[FCount-1];
end;

function TGeometryBuffers.Add(const AGeometry: TGeometryBuffer): PGeometryBuffer;
begin
  Result := Add();
  Result^ := AGeometry;
end;

procedure TGeometryBuffers.Clear;
begin
  FCount := 0;
end;

function TGeometryBuffers.GetGeometry(Index: Integer): PGeometryBuffer;
begin
  Result := @FBuffers[Index];
end;

end.
