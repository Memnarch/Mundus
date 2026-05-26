unit Mundus.Renderer.Worker.Buffer;

interface

uses
  Winapi.Windows,
  Mundus.Math,
  Mundus.DrawCall;

type
  TRenderWorkerBuffer = record
  private
    FIndices: TArray<Integer>;
    FVertexCount: Integer;
    FVertices: TArray<TFloat4>;
    FIndexCount: Integer;
    FMaxY: Integer;
    FMinY: Integer;
    FAttributes: TArray<Single>;
    FAttributesPerVertex: Integer;
    function GetAttributes(Index: Integer): PSingle;
  public
    procedure Reset;
    procedure InitBuffers(const AVertexCount, AIndexCount: Integer; const AAttributeBufferSize: Integer);
    procedure AddIndices(const AIndices: array of Integer);
    function AddVertex(const AVertex: TFloat4): Integer;
    property Vertices: TArray<TFloat4> read FVertices;
    property VertexCount: Integer read FVertexCount;
    property Indices: TArray<Integer> read FIndices;
    property IndexCount: Integer read FIndexCount;
    property Attributes[Index: Integer]: PSingle read GetAttributes;
    property AttributesPerVertex: Integer read FAttributesPerVertex;
    property MinY: Integer read FMinY write FMinY;
    property MaxY: Integer read FMaxY write FMaxY;
  end;

  PRenderWorkerBuffer = ^TRenderWorkerBuffer;

  TRenderWorkerBuffers = record
  private
    FCount: Integer;
    FBuffers: TArray<TRenderWorkerBuffer>;
    FUnpreparedIndex: Integer;
  public
    procedure Prepare(ACount: Integer);
    function TryGetNextUnprepared(out AIndex: Integer): Boolean;
    property Buffers: TArray<TRenderWorkerBuffer> read FBuffers;
    property Count: Integer read FCount;
  end;

  PRenderWorkerBuffers = ^TRenderWorkerBuffers;

implementation

{ TRenderWorkerBuffer }

procedure TRenderWorkerBuffer.AddIndices(const AIndices: array of Integer);
var
  LCount, LCurentCount: Integer;
begin
  LCount := Length(AIndices);
  LCurentCount := Length(FIndices);
  if FIndexCount + LCount >= LCurentCount then
    SetLength(FIndices, FIndexCount + LCount);
  CopyMemory(@FIndices[FIndexCount], @AIndices[0], LCount * SizeOf(Integer));
  Inc(FIndexCount, LCount);
end;

function TRenderWorkerBuffer.AddVertex(const AVertex: TFloat4): Integer;
var
  LAttributes: Integer;
begin
  Result := FVertexCount;
  Inc(FVertexCount);
  if Result = Length(FVertices) then
    SetLength(FVertices, FVertexCount);

  LAttributes := FVertexCount * FAttributesPerVertex;
  if LAttributes > Length(FAttributes) then
    SetLength(FAttributes, LAttributes);
  FVertices[Result] := AVertex;
end;

function TRenderWorkerBuffer.GetAttributes(Index: Integer): PSingle;
begin
  Result := @FAttributes[Index * FAttributesPerVertex];
end;

procedure TRenderWorkerBuffer.InitBuffers(const AVertexCount, AIndexCount, AAttributeBufferSize: Integer);
var
  LAttributes: Integer;
begin
  if AVertexCount > Length(FVertices) then
    SetLength(FVertices, AVertexCount);
  if AIndexCount > Length(FIndices) then
    SetLength(FIndices, AIndexCount);

  FAttributesPerVertex := AAttributeBufferSize div SizeOf(Single);
  LAttributes := Length(FVertices) * FAttributesPerVertex;

  if Length(FAttributes) < LAttributes then
    SetLength(FAttributes, LAttributes);

  FVertexCount := AVertexCount;
end;

procedure TRenderWorkerBuffer.Reset;
begin
  FVertexCount := 0;
  FIndexCount := 0;
end;

{ TRenderWorkerBuffers }

procedure TRenderWorkerBuffers.Prepare(ACount: Integer);
var
  i: Integer;
begin
  if Length(FBuffers) < ACount then
    SetLength(FBuffers, ACount);
  FCount := ACount;
  FUnpreparedIndex := -1;
end;

function TRenderWorkerBuffers.TryGetNextUnprepared(
  out AIndex: Integer): Boolean;
var
  LIndex: Integer;
begin
  LIndex := AtomicIncrement(FUnpreparedIndex);
  Result := LIndex < FCount;
  if Result then
    AIndex := LIndex;
end;

end.
