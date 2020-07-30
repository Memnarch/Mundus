unit Mundus.Camera;

interface

uses
  Mundus.Math;

type
  TCamera = class
  private
    FRotation: TMatrix4x4;
    FPosition: TFloat3;
  public
    constructor Create;
    property Position: TFloat3 read FPosition write FPosition;
    property Rotation: TMatrix4x4 read FRotation write FRotation;
  end;

implementation

{ TCamera }

constructor TCamera.Create;
begin
  inherited;
  FRotation.SetAsIdentMatrix4D;
end;

end.
