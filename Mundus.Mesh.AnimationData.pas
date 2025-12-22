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
    FRotations: TArray<TKeyFrame<TFloat3>>;
    FScales: TArray<TKeyFrame<TFloat3>>;
    FBoneIndex: Integer;
  public
    property BoneIndex: Integer read FBoneIndex write FBoneIndex;
    property Translations: TArray<TKeyFrame<TFloat3>> read FTranslations write FTranslations;
    property Rotations: TArray<TKeyFrame<TFloat3>> read FRotations write FRotations;
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

end.
