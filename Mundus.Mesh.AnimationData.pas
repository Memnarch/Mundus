unit Mundus.Mesh.AnimationData;

interface

uses
  Mundus.Math,
  System.Generics.Collections;

type
  TKeyFrame<T> = record
    Time: Single;
    Value: T;
  end;

  TBoneAnimationData = class
  private
    FTranslations: TArray<TKeyFrame<TFloat3>>;
    FRotations: TArray<TKeyFrame<TQuaternion>>;
    FScales: TArray<TKeyFrame<TFloat3>>;
    FBoneIndex: Integer;
    function SampleLinear(ATime: Single; const AFrames: TArray<TKeyFrame<TFloat3>>): TFloat3; overload;
    function SampleLinear(ATime: Single; const AFrames: TArray<TKeyFrame<TQuaternion>>): TQuaternion; overload;
  public
    property BoneIndex: Integer read FBoneIndex write FBoneIndex;
    function SampleMatrix(ATime: Single): TMatrix4x4;
    property Translations: TArray<TKeyFrame<TFloat3>> read FTranslations write FTranslations;
    property Rotations: TArray<TKeyFrame<TQuaternion>> read FRotations write FRotations;
    property Scales: TArray<TKeyFrame<TFloat3>> read FScales write FScales;
  end;

  TAnimationData = class
  private
    FName: string;
    FBones: TObjectList<TBoneAnimationData>;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Bones: TObjectList<TBoneAnimationData> read FBones;
  end;

implementation

{ TAnimationData }

constructor TAnimationData.Create;
begin
  inherited;
  FBones := TObjectList<TBoneAnimationData>.Create();
end;

destructor TAnimationData.Destroy;
begin
  FBones.Free;
  inherited;
end;

{ TBoneAnimationData }

function TBoneAnimationData.SampleLinear(ATime: Single; const AFrames: TArray<TKeyFrame<TFloat3>>): TFloat3;
var
  LStart: Integer;
  i: Integer;
begin
  LStart := 0;
  for i := 0 to High(AFrames) do
  begin
    if AFrames[i].Time > ATime then
      Break;
    LStart := i;
  end;
  Result := AFrames[LStart].Value;
end;

function TBoneAnimationData.SampleLinear(ATime: Single; const AFrames: TArray<TKeyFrame<TQuaternion>>): TQuaternion;
var
  LStart: Integer;
  i: Integer;
begin
  LStart := 0;
  for i := 0 to High(AFrames) do
  begin
    if AFrames[i].Time > ATime then
      Break;
    LStart := i;
  end;
  Result := AFrames[LStart].Value;
end;

function TBoneAnimationData.SampleMatrix(ATime: Single): TMatrix4x4;
var
  LTranslation, LScale: TFloat3;
  LRotation: TMatrix4x4;
begin
  if Assigned(FTranslations) then
    LTranslation := SampleLinear(ATime, FTranslations)
  else
    LTranslation := Float3(0, 0, 0);

  if Assigned(FRotations) then
    LRotation := TMatrix4x4.CreateRotationMatrix(SampleLinear(ATime, FRotations))
  else
    LRotation := TMatrix4x4.CreateIdentityMatrix;

  if Assigned(FScales) then
    LScale := SampleLinear(ATime, FScales)
  else
    LScale := Float3(1, 1, 1);

  Result := TMatrix4x4.CreateTranslationMatrix(LTranslation.X, LTranslation.Y, LTranslation.Z)
            * LRotation
            * TMatrix4x4.CreateScaleMatrix(LScale.X, LScale.Y, LScale.Z);
end;

end.
