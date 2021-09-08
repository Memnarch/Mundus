unit Mundus.Camera;

interface

uses
  Mundus.Math;

type
  TCamera = class
  private
    FRotation: TMatrix4x4;
    FPosition: TFloat3;
    FZNear: Single;
    FZFar: Single;
    FFOV: Single;
  public
    constructor Create;
    property Position: TFloat3 read FPosition write FPosition;
    property Rotation: TMatrix4x4 read FRotation write FRotation;
    property ZNear: Single read FZNear write FZNear;
    property ZFar: Single read FZFar write FZFar;
    property FOV: Single read FFOV write FFOV;
  end;

implementation

{ TCamera }

constructor TCamera.Create;
begin
  inherited;
  FRotation.SetAsIdentMatrix4D;
  FFOV := 0.7;
  FZNear := 1;
  FZFar := 1000;
end;

end.
