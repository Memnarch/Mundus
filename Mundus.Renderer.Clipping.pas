unit Mundus.Renderer.Clipping;

interface

uses
  Math,
  Mundus.Types,
  Mundus.Math,
  Mundus.DrawCall;

type
  TStaticIndices = array[0..7] of Integer;
  TIndexBuffer = record
  private
    FIndices: TStaticIndices;
    FCount: Integer;
  public
    procedure Add(AIndex: Integer); inline;
    procedure Clear; inline;
    property Indices: TStaticIndices read FIndices;
    property Count: Integer read FCount;
  end;

  PIndexBuffer = ^TIndexBuffer;

  TClipContext = record
  private
    FBufferA, FBufferB: TIndexBuffer;
    FOutputBuffer: PIndexBuffer;
    FInputBuffer: PIndexBuffer;
  public
    class function Create: TClipContext; static;
    procedure Swap; inline;
    property InputBuffer: PIndexBuffer read FInputBuffer;
    property OutputBuffer: PIndexBuffer read FOutputBuffer;
    property ResultBuffer: PIndexBuffer read FInputBuffer;
  end;

  PClipContext = ^TClipContext;

procedure ClipPolygon(const ACall: PDrawCall; AContext: PClipContext; AA, AB, AC: Integer);

implementation

type
  TClipPlane = (PosZ, NegZ, PosX, NegX, PosY, NegY);
  TClipPlanes = set of TClipPlane;

function ClipPlanes(const AVector: TFloat4): TClipPlanes;
begin
  Result := [];
  if AVector.W - AVector.X < 0 then
    Include(Result, PosX);
  if AVector.X + AVector.W < 0 then
    Include(Result, NegX);
  if AVector.W - AVector.Y < 0 then
    Include(Result, PosY);
  if AVector.Y + AVector.W < 0 then
    Include(Result, NegY);
  if AVector.W - AVector.Z < 0 then
    Include(Result, PosZ);
  if AVector.Z + AVector.W < 0 then
    Include(Result, NegZ);
end;

const
  COne: Single = 1;
procedure InterpolateVertex(A, B: PSingle; AFactor: PSingle; AOut: PSingle);
asm
  movups XMM0, [A]
  movups XMM1, [B]
  movss XMM2, [AFactor]
  shufps XMM2, XMM2, 0
  movss XMM3, [COne]
  shufps XMM3, XMM3, 0
  subps XMM3, XMM2

  mulps XMM0, XMM3
  mulps XMM1, XMM2
  addps XMM0, XMM1
  //lea eax, [AOut]
  mov eax, [AOut]
  movups [eax], XMM0
end;

function AddInterpolatedVertex(const ACall: PDrawCall; APrevIndex, AIndex: Integer; AT: Single): Integer;
var
  LPrevVertex, LVertex: PFloat4;
  LNewVertex: TFloat4;
  LPrevAttributes, LAttributes, LNewAttributes: PSingle;
  LPrevValue, LValue, LNewValue: PSingle;
  i, LCount: Integer;
  LInvAT: Single;
begin
  LPrevVertex := @ACall.Vertices[APrevIndex];
  LVertex := @ACall.Vertices[AIndex];

  LInvAT := 1.0 - AT;
  InterpolateVertex(PSingle(LPrevVertex), PSingle(LVertex), @AT, @LNewVertex);

  LPrevAttributes := ACall.Attributes[APrevIndex];
  LAttributes := ACall.Attributes[AIndex];
  Result := ACall.AddVertex(LNewVertex);
  LNewAttributes := ACall.Attributes[Result];
  LCount := ACall.AttributesPerVertex;
  if LCount > 0 then
  begin
    LPrevValue := LPrevAttributes;
    LValue := LAttributes;
    LNewValue := LNewAttributes;
    for i := 1 to LCount do
    begin
      LNewValue^ := LPrevValue^ * LInvAT + LValue^ * AT;
      Inc(LPrevValue);
      Inc(LValue);
      Inc(LNewValue);
    end;
  end;

end;

procedure Add(var AItems: TArray<Integer>; AValue: Integer);
begin
  SetLength(AItems, Length(AItems) + 1);
  AItems[High(AItems)] := AValue;
end;

procedure ClipPlane(const ACall: PDrawCall; const AContext: PClipContext; AA, AB, AC, AD: Single);
var
  LPrevIndex, LIndex, LNewIndex: Integer;
  i: Integer;
  LPrevDotProduct, LDotProduct, LT: Single;
  LPrevVertex, LVertex: TFloat4;
begin
  if AContext.InputBuffer.Count < 3 then
    Exit;

  AContext.OutputBuffer.Clear;

  LPrevIndex := AContext.InputBuffer.Indices[0];
  AContext.InputBuffer.Add(LPrevIndex);

  LPrevVertex := ACall.Vertices[LPrevIndex];
  LPrevDotProduct := AA * LPrevVertex.X + AB * LPrevVertex.Y + AC  * LPrevVertex.Z + AD * LPrevVertex.W;

  for i := 1 to Pred(AContext.InputBuffer.Count) do
  begin
    LIndex := AContext.InputBuffer.Indices[i];
    LVertex := ACall.Vertices[LIndex];
    LDotProduct :=  AA * LVertex.X + AB * LVertex.Y + AC  * LVertex.Z + AD * LVertex.W;

    if LPrevDotProduct >= 0 then
      AContext.OutputBuffer.Add(LPrevIndex);

    if Sign(LDotProduct) <> Sign(LPrevDotProduct) then
    begin
      if LDotProduct < 0 then
        LT := LPrevDotProduct / (LPrevDotProduct - LDotProduct)
      else
        LT := -LPrevDotProduct / (LDotProduct - LPrevDotProduct);

      LNewIndex := AddInterpolatedVertex(ACall, LPrevIndex, LIndex, LT);
      AContext.OutputBuffer.Add(LNewIndex);
    end;

    LPrevIndex := LIndex;
    LPrevDotProduct := LDotProduct;
  end;
  AContext.Swap;
end;

procedure ClipPolygon(const ACall: PDrawCall; AContext: PClipContext; AA, AB, AC: Integer);
var
  LPlanes: TClipPlanes;
begin
  AContext.InputBuffer.Clear;
  AContext.InputBuffer.Add(AA);
  AContext.InputBuffer.Add(AB);
  AContext.InputBuffer.Add(AC);
  LPlanes := ClipPlanes(ACall.Vertices[AA]) + ClipPlanes(ACall.Vertices[AB]) + ClipPlanes(ACall.Vertices[AC]);
  if LPlanes <> [] then
  begin
    if PosX in LPlanes then
      ClipPlane(ACall, AContext, -1, 0, 0, 1);
    if NegX in LPlanes then
      ClipPlane(ACall, AContext, 1, 0, 0, 1);
    if PosY in LPlanes then
      ClipPlane(ACall, AContext, 0, -1, 0, 1);
    if NegY in LPlanes then
      ClipPlane(ACall, AContext, 0, 1, 0, 1);
    if PosZ in LPlanes then
      ClipPlane(ACall, AContext, 0, 0, -1, 1);
    if NegZ in LPlanes then
      ClipPlane(ACall, AContext, 0, 0, 1, 1);
  end;
end;

{ TIndexBuffer }

procedure TIndexBuffer.Add(AIndex: Integer);
begin
  FIndices[FCount] := AIndex;
  Inc(FCount);
end;

procedure TIndexBuffer.Clear;
begin
  FCount := 0;
end;

{ TClipContext }

class function TClipContext.Create: TClipContext;
begin
  Result.FInputBuffer := @Result.FBufferA;
  Result.FOutputBuffer := @Result.FBufferB;
end;

procedure TClipContext.Swap;
var
  LTemp: PIndexBuffer;
begin
  LTemp := FInputBuffer;
  FInputBuffer := FOutputBuffer;
  FOutputBuffer := LTemp;
end;

end.
