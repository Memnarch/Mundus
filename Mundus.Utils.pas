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
begin
  Result := TMatrix4x4.CreateTranslationMatrix(APosition.X, APosition.Y, APosition.Z)
            * TMatrix4x4.CreateRotationMatrix(ARotation.X, ARotation.Y, ARotation.Z)
end;

function RGBToBGR(const AValue: TFloat3): TFloat3;
begin
  Result.X := AValue.Z;
  Result.Y := AValue.Y;
  Result.Z := AValue.X;
end;

end.
