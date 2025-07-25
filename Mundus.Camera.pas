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
    FAspectRatio: Single;
    FRotationMatrix: TMatrix4x4;
    procedure SetRotation(const Value: TFloat3);
  protected
    function GetProjectionMatrix: TMatrix4x4; virtual;
    function GetRotationMatrix: TMatrix4x4; virtual;
    function GetViewMatrix: TMatrix4x4; virtual;
  public
    constructor Create;
    property Position: TFloat3 read FPosition write FPosition;
    property Rotation: TFloat3 read FRotation write SetRotation;
    property ZNear: Single read FZNear write FZNear;
    property ZFar: Single read FZFar write FZFar;
    property FOV: Single read FFOV write FFOV;
    property AspectRatio: Single read FAspectRatio write FAspectRatio;
    property RotationMatrix: TMatrix4x4 read FRotationMatrix;
    property ViewMatrix: TMatrix4x4 read GetViewMatrix;
    property ProjectionMatrix: TMatrix4x4 read GetProjectionMatrix;
  end;

implementation

uses
  Mundus.Utils,
  System.Math;

{ TCamera }

constructor TCamera.Create;
begin
  inherited;
  FFOV := 0.7;
  FZNear := 1;
  FZFar := 10000;
end;

function TCamera.GetProjectionMatrix: TMatrix4x4;
begin
  Result.SetAsPerspectiveProjectionMatrix(FZNear, FZFar, FFOV, FAspectRatio);
end;

function TCamera.GetRotationMatrix: TMatrix4x4;
begin
  Result.SetAsRotationMatrix(DegToRad(FRotation.X), DegToRad(FRotation.Y), DegToRad(FRotation.Z));
end;

function TCamera.GetViewMatrix: TMatrix4x4;
begin
  Result.SetAsMoveMatrix(FPosition.X, FPosition.Y, FPosition.Z);
  Result.MultiplyMatrix4D(RotationMatrix);
  Result := Result.Inverse;
end;

procedure TCamera.SetRotation(const Value: TFloat3);
begin
  FRotation := Value;
  FRotationMatrix := GetRotationMatrix();
end;

end.
