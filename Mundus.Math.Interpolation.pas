unit Mundus.Math.Interpolation;

interface

uses
  Mundus.Math;

function CalculateFactorC(const A, B, C: TFloat2): Single;


function CalculateFactors(const A, B, C: TFloat2; ValA, ValB, ValC: Single): TFloat3;
procedure CalculateFactors4(const A, B, C: TFloat2; AW, BW, CW, OutScale: PSingle; ValA, ValB, ValC, TargetStepA, TargetStepB, TargetStepD: PFloat4);

implementation

//formulas
//  FStepA := y1 * (r2 - r3) + y2 * (r3 - r1) + y3 * (r1 - r2);
//  FStepB := r1 * (x2 - x3) + r2 * (x3 - x1) + r3 * (x1 - x2);
//  FStepC := x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2);
//  FStepD := -(x1 * (y2 * r3 - y3 * r2) + x2 * (y3 * r1 - y1 * r3) + x3 * (y1 * r2 - y2 * r1));

function CalculateFactors(const A, B, C: TFloat2; ValA, ValB, ValC: Single): TFloat3;
begin
  //StepA
  Result.X :=
    A.Y * (ValB - ValC) +
    B.Y * (ValC - ValA) +
    C.Y * (ValA - ValB);
  //StepB
  Result.Y :=
    ValA * (B.X - C.X) +
    ValB * (C.X - A.X) +
    ValC * (A.X - B.X);
  //StepD
  Result.Z := -(
    A.X * (B.Y * ValC - C.Y * ValB) +
    B.X * (C.Y * ValA - A.Y * ValC) +
    C.X * (A.Y * ValB - B.Y * ValA)
    );
end;

procedure CalculateFactors4(const A, B, C: TFloat2; AW, BW, CW, OutScale: PSingle; ValA, ValB, ValC, TargetStepA, TargetStepB, TargetStepD: PFloat4);
asm
  //A = EAX
  //B = EDX
  //C = ECX
  //preload A, B, C
  push ebx// we need a scratch register
  mov ebx, [ValA];
  movups xmm0, [ebx]
  mov ebx, [ValB]
  movups xmm1, [ebx]
  mov ebx, [ValC]
  movups xmm2, [ebx]

  mov ebx, [AW]
  movss xmm3, [ebx]
  shufps xmm3, xmm3, 0
  mulps xmm0, xmm3

  mov ebx, [BW]
  movss xmm3, [ebx]
  shufps xmm3, xmm3, 0
  mulps xmm1, xmm3

  mov ebx, [CW]
  movss xmm3, [ebx]
  shufps xmm3, xmm3, 0
  mulps xmm2, xmm3

  mov ebx, [OutScale]
  movss xmm7, [ebx]
  shufps xmm7, xmm7, 0

  //StepA
  //  Result.X :=
  //    A.Y * (ValB - ValC) +
  movss xmm4, [eax + TFloat2.Y]
  shufps xmm4, xmm4, 0
  movaps xmm3, xmm1
  subps xmm3, xmm2
  mulps xmm3, xmm4
  //    B.Y * (ValC - ValA) +
  movss xmm4, [edx + TFloat2.Y]
  shufps xmm4, xmm4, 0
  movaps xmm5, xmm2
  subps xmm5, xmm0
  mulps xmm5, xmm4
  addps xmm3, xmm5
  //    C.Y * (ValA - ValB);
  movss xmm4, [ecx + TFloat2.Y]
  shufps xmm4, xmm4, 0
  movaps xmm5, xmm0
  subps xmm5, xmm1
  mulps xmm5, xmm4
  addps xmm3, xmm5

  mulps xmm3, xmm7//* OutScale
  mov ebx, [TargetStepA]
  movups [ebx], xmm3

  //StepB
  //  Result.Y :=
  //    ValA * (B.X - C.X) +
  movss xmm3, [edx + TFloat2.X]
  subss xmm3, [ecx + TFloat2.X]
  shufps xmm3, xmm3, 0
  mulps xmm3, xmm0
  //    ValB * (C.X - A.X) +
  movss xmm4, [ecx + TFloat2.X]//C
  subss xmm4, [eax + TFloat2.X]
  shufps xmm4, xmm4, 0
  mulps xmm4, xmm1
  addps xmm3, xmm4
  //    ValC * (A.X - B.X);
  movss xmm4, [eax + TFloat2.X]//still A
  subss xmm4, [edx + TFloat2.X]
  shufps xmm4, xmm4, 0
  mulps xmm4, xmm2
  addps xmm3, xmm4

  mulps xmm3, xmm7//* OutScale
  mov ebx, [TargetStepB]
  movups [ebx], xmm3

  //StepD
  //  Result.Z := -(
  //    A.X * (B.Y * ValC - C.Y * ValB) +
  //    B.X * (C.Y * ValA - A.Y * ValC) +
  //    C.X * (A.Y * ValB - B.Y * ValA)
  //    );

  //A.X * (B.Y * ValC - C.Y * ValB) +
  movss xmm3, [edx + TFloat2.Y]
  shufps xmm3, xmm3, 0
  mulps xmm3, xmm2

  movss xmm4, [ecx + TFloat2.Y]
  shufps xmm4, xmm4, 0
  mulps xmm4, xmm1
  subps xmm3, xmm4

  movss xmm5, [eax + TFloat2.X]
  shufps xmm5, xmm5, 0
  mulps xmm3, xmm5

  //B.X * (C.Y * ValA - A.Y * ValC) +
  movss xmm4, [ecx + TFloat2.Y]
  shufps xmm4, xmm4, 0
  mulps xmm4, xmm0

  movss xmm5, [eax + TFloat2.Y]
  shufps xmm5, xmm5, 0
  mulps xmm5, xmm2
  subps xmm4, xmm5

  movss xmm5, [edx + TFloat2.X]
  shufps xmm5, xmm5, 0
  mulps xmm4, xmm5
  addps xmm3, xmm4

  //C.X * (A.Y * ValB - B.Y * ValA)
  movss xmm4, [eax + TFloat2.Y]
  shufps xmm4, xmm4, 0
  mulps xmm4, xmm1

  movss xmm5, [edx + TFloat2.Y]
  shufps xmm5, xmm5, 0
  mulps xmm5, xmm0
  subps xmm4, xmm5

  movss xmm5, [ecx + TFloat2.X]
  shufps xmm5, xmm5, 0
  mulps xmm4, xmm5
  addps xmm3, xmm4
  //make negative
  xorps xmm4, xmm4//zero
  subps xmm4, xmm3

  mulps xmm4, xmm7 //* OutScale
  mov ebx, [TargetStepD]
  movups [ebx], xmm4


  //epilog
  pop ebx
end;

function CalculateFactorC(const A, B, C: TFloat2): Single;
{$IFDEF CPUX86}
asm
  movss xmm0, [eax + TFloat2.Y]
  movss xmm1, [edx + TFloat2.Y]
  movss xmm2, [ecx + TFloat2.Y]

  movss xmm3, xmm1
  subss xmm3, xmm2
  mulss xmm3, [eax + TFloat2.X]

  movss xmm4, xmm2
  subss xmm4, xmm0
  mulss xmm4, [edx + TFloat2.X]
  addss xmm3, xmm4


  movss xmm5, xmm0
  subss xmm5, xmm1
  mulss xmm5, [ecx + TFloat2.X]
  addss xmm3, xmm5

  movss [Result], xmm3
end;
{$ELSE}
begin
  Result :=
    A.X * (B.Y - C.Y) +
    B.X * (C.Y - A.Y) +
    C.X * (A.Y - B.Y);
end;
{$ENDIF}


end.
