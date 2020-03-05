unit DrawCall;

interface

uses
  Generics.Collections,
  Math3D,
  BaseMesh;

type
  TDrawCall = record
  private
    FVertices: TArray<TFloat4>;
    FTriangles: TArray<TTriangle>;
    FVertexCount: Integer;
    FTriangleCount: Integer;
  public
    procedure AddVertex(const AVertex: TFloat4);
    procedure AddTriangle(const ATriangle: TTriangleClass);
    procedure Reset;
    property Vertices: TArray<TFloat4> read FVertices;
    property Triangles: TArray<TTriangle> read FTriangles;
    property VertexCount: Integer read FVertexCount;
    property TriangleCount: Integer read FTriangleCount;
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

procedure TDrawCall.AddVertex(const AVertex: TFloat4);
begin
  if FVertexCount = Length(FVertices) then
    SetLength(FVertices, Length(FVertices) + CBufferStep);
  FVertices[FVertexCount] := AVertex;
  Inc(FVertexCount);
end;

procedure TDrawCall.Reset;
begin
  FTriangleCount := 0;
  FVertexCount := 0;
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
