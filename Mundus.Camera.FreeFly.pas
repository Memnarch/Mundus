unit Mundus.Camera.FreeFly;

interface

uses
  Mundus.Math,
  Mundus.Camera;

type
  TFreeFlyCamera = class(TCamera)
  protected
    function GetRotationMatrix: TMatrix4x4; override;
  end;

implementation

uses
  System.Math;

{ TFreeFlyCamera }

{ TFreeFlyCamera }

function TFreeFlyCamera.GetRotationMatrix: TMatrix4x4;
var
  LRotationX: TMatrix4x4;
begin
  LRotationX.SetAsRotationXMatrix(DegToRad(Rotation.X));
  Result.SetAsRotationYMatrix(DegToRad(Rotation.Y));
  Result.MultiplyMatrix4D(LRotationX.Inverse);
end;

end.
