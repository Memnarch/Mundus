unit Mundus.Mesh.Skeleton;

interface

uses
  Mundus.Math,
  System.Generics.Collections;

type
  TBone = class
  private
    FPosition: TFloat3;
    FRotation: TQuaternion;
    FScale: TFloat3;
    FChildren: TObjectList<TBone>;
    function GetChildren: TObjectList<TBone>;
    function GetHasChildren: Boolean;
    function GetTransform: TMatrix4x4;
  public
    destructor Destroy; override;
    property Position: TFloat3 read FPosition write FPosition;
    property Rotation: TQuaternion read FRotation write FRotation;
    property Scale: TFloat3 read FScale write FScale;
    property Transform: TMatrix4x4 read GetTransform;
    property HasChildren: Boolean read GetHasChildren;
    property Children: TObjectList<TBone> read GetChildren;
  end;

  TSkeleton = class
  private
    FBones: TObjectList<TBone>;
    FInverseBindingMatrices: TArray<TMatrix4x4>;
  public
    constructor Create;
    destructor Destroy; override;
    property Bones: TObjectList<TBone> read FBones;
    property InverseBindingMatrices: TArray<TMatrix4x4> read FInverseBindingMatrices write FInverseBindingMatrices;
  end;

implementation

{ TSkeleton }

constructor TSkeleton.Create;
begin
  inherited;
  FBones := TObjectList<TBone>.Create();
end;

destructor TSkeleton.Destroy;
begin
  FBones.Free;
  inherited;
end;

{ TBone }

destructor TBone.Destroy;
begin
  FChildren.Free;
  inherited;
end;

function TBone.GetChildren: TObjectList<TBone>;
begin
  if not Assigned(FChildren) then
    FChildren := TObjectList<TBone>.Create(False);
  Result := FChildren;
end;

function TBone.GetHasChildren: Boolean;
begin
  Result := Assigned(FChildren) and not FChildren.IsEmpty;
end;

function TBone.GetTransform: TMatrix4x4;
begin
  Result := TMatrix4x4.CreateTranslationMatrix(FPosition.X, FPosition.Y, FPosition.Z)
            * TMatrix4x4.CreateRotationMatrix(FRotation)
            * TMatrix4x4.CreateScaleMatrix(FScale.X, FScale.Y, FScale.Z);
end;

end.
