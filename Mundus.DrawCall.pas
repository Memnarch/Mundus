unit Mundus.DrawCall;

interface

uses
  Generics.Collections,
  SysUtils,
  Windows,
  Mundus.Math,
  Mundus.Types,
  Mundus.Shader,
  Mundus.ValueBuffer;

type
  PSingle = System.PSingle;

  TDrawCall = record
  private
    FVertices: TArray<TFloat4>;
    FTriangles: TArray<TTriangle>;
    FVertexCount: Integer;
    FTriangleCount: Integer;
    FAttributes: TArray<Single>;
    FShader: PShaderInfo;
    FConstantValues: TArray<Byte>;
    FAttributesPerVertex: Integer;
    FVertexIndices: TArray<Int32>;
    FValues: TArray<Byte>;
    function GetAttributes(Index: Integer): PSingle;
    procedure SetShader(const Value: PShaderInfo);
  public
    function AddVertex(const AVertex: TFloat4; AAttributes: PSingle = nil): Integer;
    procedure UpdateVertex(const AIndex: Integer; const AVertex: TFloat4; AAttributes: PSingle = nil);
    procedure AddTriangle(const ATriangle: PTriangle);
    procedure Reset;
    procedure InitBuffers(AVertices: Integer);
    property Vertices: TArray<TFloat4> read FVertices;
    property VertexIndices: TArray<Int32> read FVertexIndices write FVertexIndices;
    property Attributes[Index: Integer]: PSingle read GetAttributes;
    property Triangles: TArray<TTriangle> read FTriangles;
    property VertexCount: Integer read FVertexCount;
    property TriangleCount: Integer read FTriangleCount;
    property Shader: PShaderInfo read FShader write SetShader;
    property ConstantValues: TArray<Byte> read FConstantValues write FConstantValues;
    property Values: TArray<Byte> read FValues write FValues;
    property AttributesPerVertex: Integer read FAttributesPerVertex;
  end;

  PDrawCall = ^TDrawCall;

  TDrawCalls = class
  private
    FDrawCalls: TArray<TDrawCall>;
    FCallCount: Integer;
    FUnpreparedIndex: Integer;
    function GetCalls(Index: Integer): PDrawCall;
  public
    function Add: PDrawCall;
    procedure Reset;
    function TryGetUnpreparedCall(out ACall: PDrawCall): Boolean;
    property Calls[Index: Integer]: PDrawCall read GetCalls; default;
    property Count: Integer read FCallCount;
  end;

implementation

const
  CBufferStep  = 1;

{ TDrawCall }

{ TDrawCall }

procedure TDrawCall.AddTriangle(const ATriangle: PTriangle);
begin
  if FTriangleCount = Length(FTriangles) then
    SetLength(FTriangles, Length(FTriangles) + CBufferStep);
  FTriangles[FTriangleCount] := ATriangle^;
  Inc(FTriangleCount);
end;

function TDrawCall.AddVertex(const AVertex: TFloat4; AAttributes: PSingle): Integer;
begin
  if FVertexCount = Length(FVertices) then
  begin
    SetLength(FVertices, Length(FVertices) + CBufferStep);
    SetLength(FAttributes, Length(FVertices) * FAttributesPerVertex);
  end;
  UpdateVertex(FVertexCount, AVertex, AAttributes);
  Result := FVertexCount;
  Inc(FVertexCount);
end;

function TDrawCall.GetAttributes(Index: Integer): PSingle;
begin
  Result := @FAttributes[Index * FAttributesPerVertex];
end;

procedure TDrawCall.InitBuffers(AVertices: Integer);
begin
  SetLength(FVertices, AVertices);
  SetLength(FAttributes, AVertices * FAttributesPerVertex);
end;

procedure TDrawCall.Reset;
begin
  FTriangleCount := 0;
  FVertexCount := 0;
end;

procedure TDrawCall.SetShader(const Value: PShaderInfo);
begin
  FShader := Value;
  if Assigned(FShader) then
    FAttributesPerVertex := FShader.FragmentAttributeSize div SizeOf(Single)
  else
    FAttributesPerVertex := 0;
end;

procedure TDrawCall.UpdateVertex(const AIndex: Integer; const AVertex: TFloat4; AAttributes: PSingle);
begin
  FVertices[AIndex] := AVertex;
  if Assigned(AAttributes) then
    CopyMemory(Attributes[AIndex], AAttributes, FShader.FragmentAttributeSize);
end;

{ TDrawCalls }

function TDrawCalls.Add: PDrawCall;
begin
  if FCallCount = Length(FDrawCalls) then
    SetLength(FDrawCalls, Length(FDrawCalls) + CBufferStep);
  Result := @FDrawCalls[FCallCount];
  Result.Reset;
  Inc(FCallCount);
end;

function TDrawCalls.GetCalls(Index: Integer): PDrawCall;
begin
  Result := @FDrawCalls[Index];
end;

procedure TDrawCalls.Reset;
begin
  FCallCount := 0;
  FUnpreparedIndex := -1;
end;

function TDrawCalls.TryGetUnpreparedCall(out ACall: PDrawCall): Boolean;
var
  LIndex: Integer;
begin
  LIndex := AtomicIncrement(FUnpreparedIndex);
  Result := LIndex < FCallCount;
  if Result then
    ACall := @FDrawCalls[LIndex];
end;

end.
