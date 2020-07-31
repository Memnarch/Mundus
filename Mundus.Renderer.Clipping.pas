unit Mundus.Renderer.Clipping;

interface

uses
  Math,
  Mundus.Types,
  Mundus.Math,
  Mundus.DrawCall;

function ClipPolygon(const ACall: PDrawCall; AA, AB, AC: Integer): TArray<Integer>;

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

function AddInterpolatedVertex(const ACall: PDrawCall; APrevIndex, AIndex: Integer; AT: Single): Integer;
var
  LPrevVertex, LVertex, LNewVertex: TFloat4;
  LPrevAttributes, LAttributes, LNewAttributes: TArray<Byte>;
  LPrevValue, LValue, LNewValue: PSingle;
  i, LCount: Integer;
begin
  LPrevVertex := ACall.Vertices[APrevIndex];
  LVertex := ACall.Vertices[AIndex];

  LNewVertex.X := LPrevVertex.X * (1.0 - AT) + LVertex.X * AT;
  LNewVertex.Y := LPrevVertex.Y * (1.0 - AT) + LVertex.Y * AT;
  LNewVertex.Z := LPrevVertex.Z * (1.0 - AT) + LVertex.Z * AT;
  LNewVertex.W := LPrevVertex.W * (1.0 - AT) + LVertex.W * AT;

  LPrevAttributes := ACall.Attributes[APrevIndex];
  LAttributes := ACall.Attributes[AIndex];
  SetLength(LNewAttributes, Length(LPrevAttributes));
  LCount := Length(LNewAttributes) div SizeOf(Single);
  if LCount > 0 then
  begin
    LPrevValue := @LPrevAttributes[0];
    LValue := @LAttributes[0];
    LNewValue := @LNewAttributes[0];
    for i := 1 to LCount do
    begin
      LNewValue^ := LPrevValue^ * (1.0 - AT) + LValue^ * AT;
      Inc(LPrevValue);
      Inc(LValue);
      Inc(LNewValue);
    end;
  end;
  Result := ACall.AddVertex(LNewVertex, LNewAttributes);
end;

procedure Add(var AItems: TArray<Integer>; AValue: Integer);
begin
  SetLength(AItems, Length(AItems) + 1);
  AItems[High(AItems)] := AValue;
end;

//function Sign(Value: Single): TValueSign;
//begin
//  if Value > 0 then
//    Result := 1
//  else if Value < 0 then
//    Result := -1
//  else
//    Result := 0;
//end;

procedure ClipPlane(const ACall: PDrawCall; var AIndices: TArray<Integer>; AA, AB, AC, AD: Single);
var
  LOutput: TArray<Integer>;
  LPrevIndex, LIndex, LNewIndex: Integer;
  i: Integer;
  LPrevDotProduct, LDotProduct, LT: Single;
  LPrevVertex, LVertex: TFloat4;
begin
  if Length(AIndices) < 3 then
    Exit;

  LPrevIndex := AIndices[0];
  Add(AIndices, LPrevIndex);

  LPrevVertex := ACall.Vertices[LPrevIndex];
  LPrevDotProduct := AA * LPrevVertex.X + AB * LPrevVertex.Y + AC  * LPrevVertex.Z + AD * LPrevVertex.W;

  for i := 1 to High(AIndices) do
  begin
    LIndex := AIndices[i];
    LVertex := ACall.Vertices[LIndex];
    LDotProduct :=  AA * LVertex.X + AB * LVertex.Y + AC  * LVertex.Z + AD * LVertex.W;

    if LPrevDotProduct >= 0 then
      Add(LOutput, LPrevIndex);

    if Sign(LDotProduct) <> Sign(LPrevDotProduct) then
    begin
      if LDotProduct < 0 then
        LT := LPrevDotProduct / (LPrevDotProduct - LDotProduct)
      else
        LT := -LPrevDotProduct / (LDotProduct - LPrevDotProduct);

      LNewIndex := AddInterpolatedVertex(ACall, LPrevIndex, LIndex, LT);
      Add(LOutput, LNewIndex);
    end;

    LPrevIndex := LIndex;
    LPrevDotProduct := LDotProduct;
  end;
  AIndices := LOutput;
end;

function ClipPolygon(const ACall: PDrawCall; AA, AB, AC: Integer): TArray<Integer>;
var
  LPlanes: TClipPlanes;
begin
  Result := [AA, AB, AC];
  LPlanes := ClipPlanes(ACall.Vertices[AA]) + ClipPlanes(ACall.Vertices[AB]) + ClipPlanes(ACall.Vertices[AC]);
  if LPlanes <> [] then
  begin
    if PosX in LPlanes then
      ClipPlane(ACall, Result, -1, 0, 0, 1);
    if NegX in LPlanes then
      ClipPlane(ACall, Result, 1, 0, 0, 1);
    if PosY in LPlanes then
      ClipPlane(ACall, Result, 0, -1, 0, 1);
    if NegY in LPlanes then
      ClipPlane(ACall, Result, 0, 1, 0, 1);
    if PosZ in LPlanes then
      ClipPlane(ACall, Result, 0, 0, -1, 1);
    if NegZ in LPlanes then
      ClipPlane(ACall, Result, 0, 0, 1, 1);
  end;
end;

end.
