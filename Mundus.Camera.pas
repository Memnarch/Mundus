unit Mundus.Camera;

interface

uses
  Mundus.Math;

type
  TCamera = class
  private
    FRotation: TFloat3;
    FPosition: TFloat3;
  public
    property Position: TFloat3 read FPosition write FPosition;
    property Rotation: TFloat3 read FRotation write FRotation;
  end;

implementation

end.
