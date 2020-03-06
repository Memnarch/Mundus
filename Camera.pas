unit Camera;

interface

uses
  Math3D;

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
