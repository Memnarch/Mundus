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
begin
  Result :=   TMatrix4x4.CreateRotationYMatrix(Rotation.Y)
            * TMatrix4x4.CreateRotationXMatrix(Rotation.X).Inverse;
end;

end.
