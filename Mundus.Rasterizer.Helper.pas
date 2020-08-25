unit Mundus.Rasterizer.Helper;

interface

uses
  Mundus.Types,
  Mundus.Math,
  Mundus.Math.Interpolation,
  Mundus.Rasterizer.Types;

procedure EvalHalfspace(const AConstants: PHalfEdgeConstants; const ADeltas: PHalfSpaceDeltas; ACorners: PBlockCorners; AState: PBlockState);
procedure DenormalizeFactors4(ATarget, ASource: PSingle; AZ: Single);
procedure InitFactors4(
        const ABase: PSingle;
        const AMultiplier: Integer;
        const ATarget: PSingle;
        const AAdd: PSingle
      );

procedure InitFactors(
        const ABase: PSingle;
        const AMultiplier: Integer;
        const ATarget: PSingle;
        const AAdd: PSingle;
        const AAttributeSize: Integer
      ); inline;

procedure DenormalizeFactors(
        ATarget: PSingle;
        ASource: PSingle;
        AZ: Single;
        AAttributeSize: Integer
      ); inline;

procedure StepFactors(
        AStep: PSingle;
        ATarget: PSingle;
        const AAttributeSize: Integer
      ); inline;
procedure InterpolateAttributes4(const _AX, AY: PInteger; ATarget, AStepA, AStepB, AStepD: PSingle; AZ: Single);
procedure StepFactors4(AStep, ATarget: PSingle);
procedure InterpolateAttributes(AX, AY: Integer; ATarget, AStepA, AStepB, AStepD: PSingle; const AZValue: Single; const AAttributeSize: Integer); inline;
procedure Factorize(
  const AVectorA, AVectorB, AVectorC: TFloat4;
  AAttributeA, AAttributeB, AAttributeC: PSingle;
  AStepA, AStepB, AStepD: PSingle;
  var AVecZ: TFloat3;
  const AAttributeSize: Integer); inline;

implementation

{$PointerMath ON}
{$B+}

procedure DenormalizeFactors4(ATarget, ASource: PSingle; AZ: Single);
asm
  movups xmm0, [ASource]
  movss xmm1, [AZ]
  shufps xmm1, xmm1, 0
  rcpps xmm1, xmm1
  mulps xmm0, xmm1
  movups [ATarget], xmm0
end;

procedure DenormalizeFactors(
        ATarget: PSingle;
        ASource: PSingle;
        AZ: Single;
        AAttributeSize: Integer
      );
begin
  if AAttributeSize > 0 then
    DenormalizeFactors4(ATarget, ASource, AZ);
end;

procedure Factorize(
  const AVectorA, AVectorB, AVectorC: TFloat4;
  AAttributeA, AAttributeB, AAttributeC: PSingle;
  AStepA, AStepB, AStepD: PSingle;
  var AVecZ: TFloat3;
  const AAttributeSize: Integer);
var
  LAW, LBW, LCW: Single;
  LStepCZ: Single;
  i: Integer;
begin
  LAW := AVectorA.W;
  LBW := AVectorB.W;
  LCW := AVectorC.W;
  LStepCZ := CalculateFactorC(AVectorA, AVectorB, AVectorC);
  if LStepCZ <> 0 then
    LStepCZ := 1 / LStepCZ
  else
    LStepCZ := 1;
  AVecZ.X := CalculateFactorA(AVectorA, AVectorB, AVectorC, 1/LAW, 1/LBW, 1/LCW) * LStepCZ;
  AVecZ.Y := CalculateFactorB(AVectorA, AVectorB, AVectorC, 1/LAW, 1/LBW, 1/LCW) * LStepCZ;
  AVecZ.Z := CalculateFactorD(AVectorA, AVectorB, AVectorC, 1/LAW, 1/LBW, 1/LCW) * LStepCZ;

  for i := 0 to Pred(AAttributeSize div SizeOf(Single)) do
  begin
    AStepA[i] := CalculateFactorA(AVectorA, AVectorB, AVectorC, AAttributeA[i]/LAW, AAttributeB[i]/LBW, AAttributeC[i]/LCW) * LStepCZ;
    AStepB[i] := CalculateFactorB(AVectorA, AVectorB, AVectorC, AAttributeA[i]/LAW, AAttributeB[i]/LBW, AAttributeC[i]/LCW) * LStepCZ;
    AStepD[i] := CalculateFactorD(AVectorA, AVectorB, AVectorC, AAttributeA[i]/LAW, AAttributeB[i]/LBW, AAttributeC[i]/LCW) * LStepCZ;
  end;
end;

procedure InitFactors4(
        const ABase: PSingle;
        const AMultiplier: Integer;
        const ATarget: PSingle;
        const AAdd: PSingle
      );
asm
//  mov eax, [AMultiplier]
  CVTSI2SS xmm0, AMultiplier
//  //set all parts of xmm0 to the value in the lowest part of xmm1
  shufps xmm0, xmm0, 0
//  mov eax, [ABase];
  movups xmm1, [ABase]
  mov eax, [AAdd]
  movups xmm2, [eax]
  mulps xmm1, xmm0
  addps xmm1, xmm2
  mov eax, ATarget
  movups [eax], xmm1
end;

procedure InitFactors(
        const ABase: PSingle;
        const AMultiplier: Integer;
        const ATarget: PSingle;
        const AAdd: PSingle;
        const AAttributeSize: Integer
      );
begin
  if AAttributeSize > 0 then
    InitFactors4(ABase, AMultiplier, ATarget, AAdd);
end;

//InitFactors<TAttributes>(@LAttributesY, @LStepB, i, @LStepD);
//                      LDenormalizeZY := LStepsZ.Y * i + LStepsZ.Z;
//                      InitFactors<TAttributes>(@LAttributesX, @LStepA, k, @LAttributesY);
//                      LDenormalizeZX := LStepsZ.X * k + LDenormalizeZY;
//                      DenormalizeFactors<TAttributes>(@LAttributesDenormalized, @LAttributesX, LDenormalizeZX);
procedure InterpolateAttributes4(const _AX, AY: PInteger; ATarget, AStepA, AStepB, AStepD: PSingle; AZ: Single);
asm
  //save _AX (eax) for later
  CVTSI2SS xmm3, [_AX]
  //initfactor StepB
  mov eax, [AStepB]
  movups xmm0, [eax]
  CVTSI2SS xmm1, [AY]
  shufps xmm1, xmm1, 0
  mulps xmm0, xmm1
  mov eax, [AStepD]
  movups xmm1, [eax]
  addps xmm0, xmm1
  //initfactor StepA
  mov eax, [AStepA]
  movups xmm2, [eax]
//  mov eax, ptr dword AX

  shufps xmm3, xmm3, 0
  mulps xmm2, xmm3
  addps xmm2, xmm0
  //denormalize
  movss xmm1, [AZ]
  shufps xmm1, xmm1, 0
  rcpps xmm1, xmm1
  mulps xmm2, xmm1
  movups [ATarget], xmm2
end;

procedure InterpolateAttributes(AX, AY: Integer; ATarget, AStepA, AStepB, AStepD: PSingle; const AZValue: Single; const AAttributeSize: Integer);
begin
  if AAttributeSize > 0 then
    InterpolateAttributes4(@AX, @AY, ATarget, AStepA, AStepB, AStepD, AZValue);
end;

procedure StepFactors4(AStep, ATarget: PSingle);
asm
  movups xmm0, [ATarget]
  movups xmm1, [AStep]
  addps xmm1, xmm0
  movups [ATarget], xmm1
end;

procedure StepFactors(
        AStep: PSingle;
        ATarget: PSingle;
        const AAttributeSize: Integer
      );
begin
  if AAttributeSize > 0 then
    StepFactors4(AStep, ATarget);
end;

procedure EvalHalfspace(const AConstants: PHalfEdgeConstants; const ADeltas: PHalfSpaceDeltas; ACorners: PBlockCorners; AState: PBlockState);
var
  A00, A10, A01, A11, B00, B10, B01, B11, C00, C10, C01, C11: Boolean;
  ResultOrA, ResultAndA, ResultOrB, ResultAndB, ResultOrC, ResultAndC: Boolean;
begin
    // Evaluate half-space functions
  a00 := (AConstants.C1 + ADeltas.X12 * ACorners.Y0 - ADeltas.Y12 * ACorners.X0) > 0;
  a10 := (AConstants.C1 + ADeltas.X12 * ACorners.Y0 - ADeltas.Y12 * ACorners.X1) > 0;
  a01 := (AConstants.C1 + ADeltas.X12 * ACorners.Y1 - ADeltas.Y12 * ACorners.X0) > 0;
  a11 := (AConstants.C1 + ADeltas.X12 * ACorners.Y1 - ADeltas.Y12 * ACorners.X1) > 0;
  ResultOrA := a00 or a10 or a01 or a11;
  ResultAndA := a00 and a10 and a01 and a11;

  b00 := (AConstants.C2 + ADeltas.X23 * ACorners.Y0 - ADeltas.Y23 * ACorners.X0) > 0;
  b10 := (AConstants.C2 + ADeltas.X23 * ACorners.Y0 - ADeltas.Y23 * ACorners.X1) > 0;
  b01 := (AConstants.C2 + ADeltas.X23 * ACorners.Y1 - ADeltas.Y23 * ACorners.X0) > 0;
  b11 := (AConstants.C2 + ADeltas.X23 * ACorners.Y1 - ADeltas.Y23 * ACorners.X1) > 0;
  ResultOrB := B00 or B10 or B01 or B11;
  ResultAndB := B00 and B10 and B01 and B11;

  c00 := (AConstants.C3 + ADeltas.X31 * ACorners.Y0 - ADeltas.Y31 * ACorners.X0) > 0;
  c10 := (AConstants.C3 + ADeltas.X31 * ACorners.Y0 - ADeltas.Y31 * ACorners.X1) > 0;
  c01 := (AConstants.C3 + ADeltas.X31 * ACorners.Y1 - ADeltas.Y31 * ACorners.X0) > 0;
  c11 := (AConstants.C3 + ADeltas.X31 * ACorners.Y1 - ADeltas.Y31 * ACorners.X1) > 0;
  ResultOrC := C00 or C10 or C01 or C11;
  ResultAndC := C00 and C10 and C01 and C11;

  AState.Intersects := ResultOrA or ResultOrB or ResultOrC;
  AState.IsFullBlock := ResultAndA and ResultAndB and ResultAndC;
end;

end.
