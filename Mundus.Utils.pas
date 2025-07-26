unit Mundus.Utils;

interface

uses
  Mundus.Math;

function BuildTransformMatrix(const APosition, ARotation: TFloat3): TMatrix4x4;
function RGBToBGR(const AValue: TFloat3): TFloat3;

implementation

uses
  System.Math;

function BuildTransformMatrix(const APosition, ARotation: TFloat3): TMatrix4x4;
var
  LRotation: TMatrix4x4;
begin
  Result.SetAsMoveMatrix(APosition.X, APosition.Y, APosition.Z);

  LRotation.SetAsRotationXMatrix(DegToRad(ARotation.X));
  Result.MultiplyMatrix4D(LRotation);

  LRotation.SetAsRotationYMatrix(DegToRad(ARotation.Y));
  Result.MultiplyMatrix4D(LRotation);

  LRotation.SetAsRotationZMatrix(DegToRad(ARotation.Z));
  Result.MultiplyMatrix4D(LRotation);
end;

function RGBToBGR(const AValue: TFloat3): TFloat3;
begin
  Result.X := AValue.Z;
  Result.Y := AValue.Y;
  Result.Z := AValue.X;
end;

end.
