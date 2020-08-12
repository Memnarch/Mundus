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
    FAttributesPerVertex: Integer;
    FShader: TShaderClass;
    FValues: TValueBuffers;
    procedure SetShader(const Value: TShaderClass);
    function GetAttributes(Index: Integer): PSingle;
  public
    function AddVertex(const AVertex: TFloat4; AAttributes: PSingle = nil): Integer;
    procedure AddTriangle(const ATriangle: PTriangle);
    procedure Reset;
    procedure InitBuffers(AVertices: Integer);
    property Vertices: TArray<TFloat4> read FVertices;
    property Attributes[Index: Integer]: PSingle read GetAttributes;
    property Triangles: TArray<TTriangle> read FTriangles;
    property VertexCount: Integer read FVertexCount;
    property TriangleCount: Integer read FTriangleCount;
    property Shader: TShaderClass read FShader write SetShader;
    property Values: TValueBuffers read FValues;
    property AttributesPerVertex: Integer read FAttributesPerVertex;
  end;

  PDrawCall = ^TDrawCall;

  TDrawCalls = class
  private
    FDrawCalls: TArray<TDrawCall>;
    FCallCount: Integer;
    function GetCalls(Index: Integer): PDrawCall;
  public
    function Add: PDrawCall;
    procedure Reset;
    property Calls[Index: Integer]: PDrawCall read GetCalls; default;
    property Count: Integer read FCallCount;
  end;

implementation

const
  CBufferStep  = 128;

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
  FVertices[FVertexCount] := AVertex;
  if Assigned(AAttributes) then
    CopyMemory(Attributes[FVertexCount], AAttributes, FShader.GetAttributeBufferSize);
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
  SetLength(FAttributes, AVertices);
end;

procedure TDrawCall.Reset;
begin
  FTriangleCount := 0;
  FVertexCount := 0;
  FValues.Reset;
end;

procedure TDrawCall.SetShader(const Value: TShaderClass);
begin
  FShader := Value;
  if Assigned(FShader) then
    FAttributesPerVertex := FShader.GetAttributeBufferSize div SizeOf(Single)
  else
    FAttributesPerVertex := 0;
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
end;

end.
