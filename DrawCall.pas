unit DrawCall;

interface

uses
  Generics.Collections,
  Math3D,
  RenderTypes,
  Shader,
  ValueBuffer;

type
  TDrawCall = record
  private
    FVertices: TArray<TFloat4>;
    FTriangles: TArray<TTriangle>;
    FVertexCount: Integer;
    FTriangleCount: Integer;
    FAttributes: TArray<TVertexAttributeBuffer>;
    FShader: TShaderClass;
    FValues: TValueBuffers;
  public
    procedure AddVertex(const AVertex: TFloat4; const AAttributes: TVertexAttributeBuffer);
    procedure AddTriangle(const ATriangle: TTriangleClass);
    procedure Reset;
    property Vertices: TArray<TFloat4> read FVertices;
    property Attributes: TArray<TVertexAttributeBuffer> read FAttributes;
    property Triangles: TArray<TTriangle> read FTriangles;
    property VertexCount: Integer read FVertexCount;
    property TriangleCount: Integer read FTriangleCount;
    property Shader: TShaderClass read FShader write FShader;
    property Values: TValueBuffers read FValues;
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

procedure TDrawCall.AddTriangle(const ATriangle: TTriangleClass);
var
  LTriangle: PTriangle;
begin
  if FTriangleCount = Length(FTriangles) then
    SetLength(FTriangles, Length(FTriangles) + CBufferStep);
  LTriangle := @FTriangles[FTriangleCount];
  LTriangle.VertexA := ATriangle.VertexA;
  LTriangle.VertexB := ATriangle.VertexB;
  LTriangle.VertexC := ATriangle.VertexC;
  LTriangle.UVA := ATriangle.UVA;
  LTriangle.UVB := ATriangle.UVB;
  LTriangle.UVC := ATriangle.UVC;
  Inc(FTriangleCount);
end;

procedure TDrawCall.AddVertex(const AVertex: TFloat4; const AAttributes: TVertexAttributeBuffer);
begin
  if FVertexCount = Length(FVertices) then
  begin
    SetLength(FVertices, Length(FVertices) + CBufferStep);
    SetLength(FAttributes, Length(FVertices));
  end;
  FVertices[FVertexCount] := AVertex;
  FAttributes[FVertexCount] := Copy(AAttributes, 0);
  Inc(FVertexCount);
end;

procedure TDrawCall.Reset;
begin
  FTriangleCount := 0;
  FVertexCount := 0;
  FValues.Reset;
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
