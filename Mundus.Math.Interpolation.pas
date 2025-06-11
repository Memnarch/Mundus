unit Mundus.Math.Interpolation;

interface

uses
  Mundus.Math;

function CalculateFactorC(const A, B, C: TFloat2): Single;


function CalculateFactors(const A, B, C: TFloat2; ValA, ValB, ValC: Single): TFloat3;

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

function CalculateFactorC(const A, B, C: TFloat2): Single;
begin
  Result :=
    A.X * (B.Y - C.Y) +
    B.X * (C.Y - A.Y) +
    C.X * (A.Y - B.Y);
end;


end.
