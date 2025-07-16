unit Mundus.Camera;

interface

uses
  Mundus.Math;

type
  TCamera = class
  private
    FRotation: TFloat3;
    FPosition: TFloat3;
    FZNear: Single;
    FZFar: Single;
    FFOV: Single;
  public
    constructor Create;
    property Position: TFloat3 read FPosition write FPosition;
    property Rotation: TFloat3 read FRotation write FRotation;
    property ZNear: Single read FZNear write FZNear;
    property ZFar: Single read FZFar write FZFar;
    property FOV: Single read FFOV write FFOV;
  end;

implementation

{ TCamera }

constructor TCamera.Create;
begin
  inherited;
  FFOV := 0.7;
  FZNear := 1;
  FZFar := 10000;
end;

end.
