unit Mundus.Mesh.Loader.Gltf;

interface

uses
  Mundus.Mesh.Loader,
  Mundus.Mesh,
  System.JSON,
  Mundus.Math,
  Mundus.Types,
  Mundus.Material,
  Mundus.Mesh.Loader.Gltf.Types,
  Mundus.Mesh.Skeleton,
  System.Generics.Collections,
  Mundus.Mesh.AnimationData;

type
  TGLTFMeshLoader = class(TAbstractMeshLoader)
  private
    class function ReadAnimationChannel(const AData: TGLTFData;
      const AAnimation: PAnimation; const AChannel: TChannel;
      AScale: Single): TArray<TKeyFrame<TFloat3>>; static;
  protected
    class function ReadBuffers(const ADoc: TJSONObject; const ADirectory: string): TArray<TBuffer>;
    class function ReadBufferViews(const ADoc: TJSONObject): TArray<TBufferView>;
    class function ReadAccessors(const ADoc: TJSONObject): TArray<TAccessor>;
    class function ReadTextures(const ADoc: TJSONObject): TArray<TTexture>;
    class function ReadImages(const ADoc: TJSONObject): TArray<TImage>;
    class function ReadMaterials(const ADoc: TJSONObject): TArray<TMaterial>;
    class function ReadNodes(const ADoc: TJSONObject): TArray<TNode>;
    class function ReadSkins(const ADoc: TJSONObject): TArray<TSkin>;
    class function ReadAnimations(const ADoc: TJSONObject): TArray<TAnimation>;
    class function ReadChannels(const AValues: TJSONArray): TArray<TChannel>;
    class function ReadSamplers(const AValues: TJSONArray): TArray<TSampler>;
    class function Read<T>(const AData: TGLTFData; const AAccessor: Integer): TArray<T>; overload;
    class procedure ReadMeshes(const ADoc: TJSONObject; const AData: TGLTFData; const ATarget: TMeshGroup);
    class function BuildJoints(const AIndices: TArray<TJointIndices>; const AWeights: TArray<TFloat4>): TArray<TJoints>;
    class procedure BuildSkeleton(const AData: TGLTFData; ATarget: TMeshGroup; ANodeToBone: TDictionary<Integer, Integer>);
    class procedure BuildAnimationData(const AData: TGLTFData; ATarget: TMeshGroup; ANodeToBone: TDictionary<Integer, Integer>);
    class function ReadAnimationRotationChannel(const AData: TGLTFData; const AAnimation: PAnimation; const AChannel: TChannel): TArray<TKeyFrame<TFloat3>>;
  public
    class function CanLoad(const AFileName: string): Boolean; override;
    class function LoadFromFile(const AFileName: string): TMeshGroup; override;
  end;

function ElementCount(AType: TElementType): Integer;
function ComponentSize(AType: TComponentType): Integer;

implementation

uses
  System.StrUtils,
  System.IOUtils,
  System.SysUtils,
  Winapi.Windows,
  System.Math,
  System.Types;

type
  TValue4<T> = array[0..3] of T;
  TValue3<T> = array[0..2] of T;
  TPathTarget = (ptUnknown, ptTranslation, ptRotation, ptScale);

const
  CMeterToCM = 100;

function ConvertPathTarge(const APath: string): TPathTarget;
begin
  case AnsiIndexText(APath, ['translation', 'rotation', 'scale']) of
    0: Result := ptTranslation;
    1: Result := ptRotation;
    2: Result := ptScale;
  else
    Result := ptUnknown;
  end;
end;
procedure RaiseInvalidComponentSize;
begin
  raise Exception.Create('InvalidComponentsize') at ReturnAddress;
end;

procedure RaiseInvalidElementCount;
begin
  raise Exception.Create('InvalidElementCount') at ReturnAddress;
end;

function ComponentSize(AType: TComponentType): Integer;
begin
  case AType of
    ctByte, ctUByte: Result := 1;
    ctShort, ctUShort: Result := 2;
    ctUInt32, ctFloat32: Result := 4;
  else
    RaiseInvalidComponentSize;
    Result := 0;//make compiler happy
  end;
end;

function ElementCount(AType: TElementType): Integer;
begin
  case AType of
    etScalar: Result := 1;
    etVec2: Result := 2;
    etVec3: Result := 3;
    etVec4: Result := 4;
    etMat2: Result := 2 * 2;
    etMat3: Result := 3 * 3;
    etMat4: Result := 4 * 4;
  else
    RaiseInvalidElementCount;
    Result := 0;//make compiler happy
  end;
end;

//https://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToEuler/
function QuaternionToEuler(const Q: TFloat4): TFloat3;
var
  LCheck: Single;
begin
  Result.X := ArcSin(2 * Q.X * Q.Y + 2 * Q.Z * Q.W);
  LCheck := Q.X * Q.Y + Q.Z * Q.W;
  if LCheck = 0.5 then
  begin
    Result.Y := 2 * ArcTan2(Q.X, Q.W);
    Result.Z := 0;
  end
  else if LCheck = -0.5 then
  begin
    Result.Y := -2 * ArcTan2(Q.X, Q.W);
    Result.Z := 0;
  end
  else
  begin
    Result.Y := ArcTan2(2 * Q.Y * Q.W - 2 * Q.X * Q.Z, 1 - 2 * Q.Y * Q.Y - 2 * Q.Z * Q.Z);
    Result.Z := ArcTan2(2 * Q.X * Q.W - 2 * Q.Y * Q.Z, 1 - 2 * Q.X * Q.X - 2 * Q.Z * Q.Z);
  end;

  Result.X := RadToDeg(Result.X);
  Result.Y := RadToDeg(Result.Y);
  Result.Z := RadToDeg(Result.Z);
end;

{ TGLTFMeshLoader }

class procedure TGLTFMeshLoader.BuildAnimationData(const AData: TGLTFData; ATarget: TMeshGroup; ANodeToBone: TDictionary<Integer, Integer>);
var
  LAnimationByBone: TDictionary<Integer, TBoneAnimationData>;
  LAnimation: PAnimation;
  LAnimData: TAnimationData;
  LBoneAnim: TBoneAnimationData;
  LChannel: TChannel;
  LChannelTarget: TPathTarget;
  LChannelData: TKeyFrame<TFloat3>;
  i: Integer;
  LBoneIndex: Integer;
begin
  LAnimationByBone := TDictionary<Integer, TBoneAnimationData>.Create();
  try
    for i := 0 to High(AData.Animations) do
    begin
      LAnimation := @AData.Animations[i];
      LAnimData := TAnimationData.Create();
      ATarget.Animations.Add(LAnimData);
      LAnimData.Name := LAnimation.Name;
      LAnimationByBone.Clear;
      for LChannel in LAnimation.Channels do
      begin
        LBoneIndex := ANodeToBone[LChannel.Target.Node];
        if not LAnimationByBone.TryGetValue(LBoneIndex, LBoneAnim) then
        begin
          LBoneAnim := TBoneAnimationData.Create();
          LBoneAnim.BoneIndex := LBoneIndex;
          LAnimData.Bones.Add(LBoneAnim);
          LAnimationByBone.Add(LBoneIndex, LBoneAnim);
        end;

        LChannelTarget := ConvertPathTarge(LChannel.Target.Path);
        case LChannelTarget of
          ptTranslation: LBoneAnim.Translations := ReadAnimationChannel(AData, LAnimation, LChannel, CMeterToCM);
          ptRotation: LBoneAnim.Rotations := ReadAnimationRotationChannel(AData, LAnimation, LChannel);
          ptScale: LBoneAnim.Scales := ReadAnimationChannel(AData, LAnimation, LChannel, 1);
        else
          Continue;
        end;
      end;
    end;
  finally
    LAnimationByBone.Free;
  end;
end;

class function TGLTFMeshLoader.BuildJoints(
  const AIndices: TArray<TJointIndices>;
  const AWeights: TArray<TFloat4>): TArray<TJoints>;
var
  i, k: Integer;
  LJoints: TJoints;
  LIndices: TJointIndices;
  LWeights: TFloat4;
begin
  SetLength(Result, Length(AIndices));
  for i := 0 to High(AIndices) do
  begin
    LJoints := Default(TJoints);
    LIndices := AIndices[i];
    LWeights := AWeights[i];
    for k := 0 to High(LIndices) do
    begin
      if LWeights.Elements[k] = 0 then break;

      Inc(LJoints.Count);
      LJoints.Values[k].Index := LIndices[k];
      LJoints.Values[k].Weight := LWeights.Elements[k];
    end;
    Result[i] := LJoints;
  end;
end;

class procedure TGLTFMeshLoader.BuildSkeleton(const AData: TGLTFData; ATarget: TMeshGroup; ANodeToBone: TDictionary<Integer, Integer>);
var
  LSkeleton: TSkeleton;
  LSkin: PSkin;
  LBone: TBone;
  i: Integer;
  LNode: PNode;
begin
  if not Assigned(Adata.Skins) then Exit;

  //for now, we support a single skeleton, only
  LSkin := @AData.Skins[0];
  LSkeleton := TSkeleton.Create();
  try
    if LSkin.InverseBindMatrices > -1 then
      LSkeleton.InverseBindingMatrices := Read<TMatrix4x4>(AData, LSkin.InverseBindMatrices);
    for i := 0 to High(LSkin.Joints) do
    begin
      LNode := @AData.Nodes[LSkin.Joints[i]];
      ANodeToBone.Add(LSkin.Joints[i], i);
      LBone := TBone.Create();
      try
        LBone.Position := LNode.Translation;
        LBone.Rotation := LNode.Rotation;
        LBone.Scale := LNode.Scale;
        LSkeleton.Bones.Add(LBone);
      except
        LBone.Free;
        raise;
      end;
    end;
    ATarget.Skeleton := LSkeleton;
  except
    LSkeleton.Free;
    raise;
  end;
end;

class function TGLTFMeshLoader.CanLoad(const AFileName: string): Boolean;
begin
  Result := EndsText('.gltf', AFileName);
end;

class function TGLTFMeshLoader.LoadFromFile(const AFileName: string): TMeshGroup;
var
  LDocument: TJSONObject;
  LData: TGLTFData;
  LNodeToBone: TDictionary<Integer, Integer>;
begin
  LDocument := TJSONObject.ParseJSONValue(TFile.ReadAllText(AFileName)) as TJSONObject;
  try
    Result := TMeshGroup.Create();
    try
      LData.Buffers := ReadBuffers(LDocument, ExtractFilePath(AFileName));
      LData.Views := ReadBufferViews(LDocument);
      LData.Accessors := ReadAccessors(LDocument);
      LData.Images := ReadImages(LDocument);
      LData.Textures := ReadTextures(LDocument);
      LData.Materials := ReadMaterials(LDocument);
      LData.Nodes := ReadNodes(LDocument);
      LData.Skins := ReadSkins(LDocument);
      LData.Animations := ReadAnimations(LDocument);
      ReadMeshes(LDocument, LData, Result);
      LNodeToBone := TDictionary<Integer, Integer>.Create();
      try
        BuildSkeleton(LData, Result, LNodeToBone);
        BuildAnimationData(LData, Result, LNodeToBone);
      finally
        LNodeToBone.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    LDocument.Free;
  end;
end;

class function TGLTFMeshLoader.ReadAccessors(const ADoc: TJSONObject): TArray<TAccessor>;
var
  LItems: TJSONArray;
  LItem: TJSONObject;
  i: Integer;
  LComponentType: TComponentType;
  LElementType: TElementType;
begin
  LItems := ADoc.GetValue<TJSONArray>('accessors');
  SetLength(Result, LItems.Count);
  for i := 0 to High(Result) do
  begin
    LItem := LItems[i] as TJSONObject;
    Result[i].BufferView := LItem.GetValue<Integer>('bufferView', 0);
    Result[i].Offset := LItem.GetValue<Int64>('byteOffset', 0);
    case LItem.GetValue<Integer>('componentType') of
      5120: LComponentType := ctByte;
      5121: LComponentType := ctUByte;
      5122: LComponentType := ctShort;
      5123: LComponentType := ctUShort;
      5125: LComponentType := ctUInt32;
      5126: LComponentType := ctFloat32;
    else
      raise Exception.Create('Unexpected component type');
    end;
    Result[i].ComponentType := LComponentType;
    Result[i].Count := LItem.GetValue<Integer>('count');
    case AnsiIndexText(LItem.GetValue<string>('type'), ['SCALAR', 'VEC2', 'VEC3', 'VEC4', 'MAT2', 'MAT3', 'MAT4']) of
      0: LElementType := etScalar;
      1: LElementType := etVec2;
      2: LElementType := etVec3;
      3: LElementType := etVec4;
      4: LElementType := etMat2;
      5: LElementType := etMat3;
      6: LElementType := etMat4;
    else
      raise Exception.Create('Unexpected element type');
    end;
    Result[i].ElementType := LElementType;
  end;
end;

class function TGLTFMeshLoader.ReadAnimationChannel(const AData: TGLTFData; const AAnimation: PAnimation; const AChannel: TChannel; AScale: Single): TArray<TKeyFrame<TFloat3>>;
var
  LTimes: TArray<Single>;
  LData: TArray<TFloat3>;
  LSampler: PSampler;
  i: Integer;
begin
  LSampler := @AAnimation.Samplers[AChannel.Sampler];
  LTimes := Read<Single>(AData, LSampler.Input);
  LData := Read<TFloat3>(AData, LSampler.Output);
  SetLength(Result, Length(LTimes));
  for i := 0 to High(Result) do
  begin
    Result[i].Time := LTimes[i];
    Result[i].Value := LData[i] * AScale;
  end;
end;

class function TGLTFMeshLoader.ReadAnimationRotationChannel(
  const AData: TGLTFData; const AAnimation: PAnimation;
  const AChannel: TChannel): TArray<TKeyFrame<TFloat3>>;
var
  LTimes: TArray<Single>;
  LData: TArray<TFloat4>;
  LSampler: PSampler;
  i: Integer;
begin
  LSampler := @AAnimation.Samplers[AChannel.Sampler];
  LTimes := Read<Single>(AData, LSampler.Input);
  LData := Read<TFloat4>(AData, LSampler.Output);
  SetLength(Result, Length(LTimes));
  for i := 0 to High(Result) do
  begin
    Result[i].Time := LTimes[i];
    Result[i].Value := QuaternionToEuler(LData[i]);
  end;
end;

class function TGLTFMeshLoader.ReadAnimations(const ADoc: TJSONObject): TArray<TAnimation>;
var
  LValues: TJSONArray;
  LValue: TJSONObject;
  LAnimation: TAnimation;
  i: Integer;
begin
  if not ADoc.TryGetValue<TJSONArray>('animations', LValues) then
    Exit(nil);

  SetLength(Result, LValues.Count);

  for i := 0 to High(Result) do
  begin
    LValue := LValues[i] as TJSONObject;
    LAnimation.Name := LValue.GetValue<string>('name', '');
    LAnimation.Channels := ReadChannels(LValue.GetValue<TJSONArray>('channels'));
    LAnimation.Samplers := ReadSamplers(LValue.GetValue<TJSONArray>('samplers'));
    Result[i] := LAnimation;
  end;
end;

class function TGLTFMeshLoader.Read<T>(const AData: TGLTFData; const AAccessor: Integer): TArray<T>;
var
  LAccessor: PAccessor;
  LBuffer: PBuffer;
  LView: PBufferView;
begin
  LAccessor := @AData.Accessors[AAccessor];
  if (ComponentSize(LAccessor.ComponentType) * ElementCount(LAccessor.ElementType)) <> SizeOf(T) then
    raise Exception.Create('Datatype missmatch');
  LView := @AData.Views[LAccessor.BufferView];
  LBuffer := @AData.Buffers[LView.BufferIndex];
  SetLength(Result, LAccessor.Count);
  CopyMemory(@Result[0], @LBuffer.Data[LView.Offset + LAccessor.Offset], LAccessor.Count * SizeOf(T));
end;

class function TGLTFMeshLoader.ReadBuffers(const ADoc: TJSONObject; const ADirectory: string): TArray<TBuffer>;
var
  LBuffers: TJSONArray;
  i: Integer;
begin
  LBuffers := ADoc.GetValue<TJSONArray>('buffers');
  SetLength(Result, LBuffers.Count);
  for i := 0 to High(Result) do
    Result[i].Data := TFile.ReadAllBytes(TPath.Combine(ADirectory, LBuffers[i].GetValue<string>('uri')));
end;

class function TGLTFMeshLoader.ReadBufferViews(const ADoc: TJSONObject): TArray<TBufferView>;
var
  LViews: TJSONArray;
  LView: TJSONObject;
  i: Integer;
begin
  LViews := ADoc.GetValue<TJSONArray>('bufferViews');
  SetLength(Result, LViews.Count);
  for i := 0 to High(Result) do
  begin
    LView := LViews[i] as TJSONObject;
    Result[i].BufferIndex := LView.GetValue<Integer>('buffer');
    Result[i].Length := LView.GetValue<Int64>('byteLength');
    Result[i].Offset := LView.GetValue<Int64>('byteOffset', 0);
  end;
end;

class function TGLTFMeshLoader.ReadChannels(const AValues: TJSONArray): TArray<TChannel>;
var
  LValue, LTarget: TJSONObject;
  LChannel: TChannel;
  i: Integer;
begin
  SetLength(Result, AValues.Count);
  for i := 0 to High(Result) do
  begin
    LValue := AValues[i] as TJSONObject;
    LChannel.Sampler := LValue.GetValue<Integer>('sampler');
    LTarget := LValue.GetValue<TJSONObject>('target');
    LChannel.Target.Node := LTarget.GetValue<Integer>('node');
    LChannel.Target.Path := LTarget.GetValue<string>('path');
    Result[i] := LChannel;
  end;
end;

class function TGLTFMeshLoader.ReadImages(const ADoc: TJSONObject): TArray<TImage>;
var
  LValues: TJSONArray;
  LValue: TJSONObject;
  i: Integer;
begin
  if not ADoc.TryGetValue<TJSONArray>('images', LValues) then
    Exit(nil);
  SetLength(Result, LValues.Count);
  for i := 0 to High(Result) do
  begin
    LValue := LValues[i] as TJSONObject;
    Result[i].MimeType := LValue.GetValue<string>('mimeType', '');
    Result[i].Name := LValue.GetValue<string>('name', '');
    Result[i].Uri := LValue.GetValue<string>('uri', '');
  end;
end;

class function TGLTFMeshLoader.ReadMaterials(const ADoc: TJSONObject): TArray<TMaterial>;
var
  LValues: TJSONArray;
  LValue, LPBR, LTextureInfo: TJSONObject;
  i: Integer;
begin
  if not ADoc.TryGetValue<TJSONArray>('materials', LValues) then
    Exit(nil);
  SetLength(Result, LValues.Count);
  for i := 0 to High(Result) do
  begin
    LValue := LValues[i] as TJSONObject;
    Result[i] := TMaterial.Create();
    Result[i].Name := LValue.GetValue<string>('name', '');
    if LValue.TryGetValue<TJSONObject>('pbrMetallicRoughness', LPBR) and LPBR.TryGetValue<TJSONObject>('baseColorTexture', LTextureInfo) then
      Result[i].DiffuseTexture := LTextureInfo.GetValue<Integer>('index');
  end;
end;

class procedure TGLTFMeshLoader.ReadMeshes(const ADoc: TJSONObject; const AData: TGLTFData; const ATarget: TMeshGroup);
var
  LItems, LPrimitives: TJSONArray;
  LItem, LPrimitive: TJSONObject;
  LAttributes: TJSONObject;
  i, k, m, LIndex: Integer;
  LIndiceAccessor: Integer;
  LIndices: TArray<Word>;
  LMesh: TMesh;
  LValue: TFloat3;
  LUV, LTempUV: TFloat2;
  LTriangle: TTriangle;
  LTextureReference: TTextureReference;
  LTexture: PTexture;
  LImage: PImage;
  LMaterial: TMaterial;
begin
  LItems := ADoc.GetValue<TJSONArray>('meshes');
  for i := 0 to Pred(LItems.Count) do
  begin
    LItem := LItems[i] as TJSONObject;
    LPrimitives := LItem.GetValue<TJSONArray>('primitives');
    for k := 0 to Pred(LPrimitives.Count) do
    begin
      LMesh := TMesh.Create();
      ATarget.Meshes.Add(LMesh);
      LPrimitive := LPrimitives[k] as TJSONObject;
      LIndiceAccessor := LPrimitive.GetValue<Integer>('indices', -1);
      if LIndiceAccessor > -1 then
        LIndices := Read<Word>(AData, LIndiceAccessor);

      if LPrimitive.TryGetValue<Integer>('material', LIndex) then
      begin
        LMaterial := AData.Materials[LIndex];
        if LMaterial.DiffuseTexture > -1 then
        begin
          LTexture := @AData.Textures[LMaterial.DiffuseTexture];
          LImage := @AData.Images[LTexture.Source];
          LTextureReference.Name := LImage.Name;
          LTextureReference.FileName := LImage.Uri;
          LMaterial.DiffuseTexture := LMesh.AddTextureReference(LTextureReference);
        end;
        LMesh.Material := LMaterial;
      end;
      LAttributes := LPrimitive.GetValue<TJSONObject>('attributes');
      for LValue in Read<TFloat3>(AData, LAttributes.GetValue<Integer>('POSITION')) do
        LMesh.AddVertice(LValue * CMeterToCM);

      if LAttributes.TryGetValue<Integer>('NORMAL', LIndex) then
        for LValue in Read<TFloat3>(AData, LIndex) do
          LMesh.AddNormal(LValue);

      //UVs need to be flipped on the Y (V) Axis. GLTFs Coordinatesystem is flipped here
      if LAttributes.TryGetValue<Integer>('TEXCOORD_0', LIndex) then
        for LUV in Read<TFloat2>(AData, LIndex) do
        begin
          LTempUV := LUV;
          LTempUV.V := 1 - LTempUV.V;
          LMesh.AddUV(LTempUV);
        end;

      if LAttributes.TryGetValue<Integer>('TEXCOORD_1', LIndex) then
        for LUV in Read<TFloat2>(AData, LIndex) do
        begin
          LTempUV := LUV;
          LTempUV.V := 1 - LTempUV.V;
          LMesh.AddUV(LTempUV, 1);
        end;

      if LAttributes.TryGetValue<Integer>('JOINTS_0', LIndex) then
      begin
        LMesh.Joints := BuildJoints(
          Read<TJointIndices>(AData, LIndex),
          Read<TFloat4>(AData, LAttributes.GetValue<Integer>('WEIGHTS_0'))
        );
      end;

      for m := 0 to Pred(Length(LIndices) div 3) do
      begin
        LTriangle.VertexA := LIndices[m * 3];
        LTriangle.VertexB := LIndices[m * 3 + 1];
        LTriangle.VertexC := LIndices[m * 3 + 2];
        LMesh.AddTriangle(LTriangle);
      end;
    end;
  end;
end;

class function TGLTFMeshLoader.ReadNodes(const ADoc: TJSONObject): TArray<TNode>;
var
  LValues: TJSONArray;
  LValue: TJSONObject;
  LNode: TNode;
  i: Integer;
  LValue4: TValue4<Single>;
  LValue3: TValue3<Single>;
begin
  if not ADoc.TryGetValue<TJSONArray>('nodes', LValues) then
    Exit(nil);

  SetLength(Result, LValues.Count);
  for i := 0 to High(Result) do
  begin
    LValue := LValues[i] as TJSONObject;
    LNode := Default(TNode);
    LNode.Name := LValue.GetValue<string>('name', '');
    LNode.Children := LValue.GetValue<TArray<Integer>>('children', nil);
    LNode.Skin := LValue.GetValue<Integer>('skin', -1);
    LNode.Mesh := LValue.GetValue<Integer>('mesh', -1);

    if LValue.TryGetValue<TValue4<Single>>('rotation', LValue4) then
      LNode.Rotation := QuaternionToEuler(TFloat4(LValue4))
    else
      LNode.Rotation := Float3(0, 0, 0);

    if LValue.TryGetValue<TValue3<Single>>('translation', LValue3) then
      LNode.Translation := TFloat3(LValue3) * CMeterToCM
    else
      LNode.Translation := Default(TFloat3);


    if LValue.TryGetValue<TValue3<Single>>('scale', LValue3) then
      LNode.Scale := TFloat3(LValue3)
    else
      LNode.Scale := Float3(1, 1, 1);
    Result[i] := LNode;
  end;
end;

class function TGLTFMeshLoader.ReadSamplers(const AValues: TJSONArray): TArray<TSampler>;
var
  LValue: TJSONObject;
  LSampler: TSampler;
  i: Integer;
begin
  SetLength(Result, AValues.Count);
  for i := 0 to High(Result) do
  begin
    LValue := AValues[i] as TJSONObject;
    LSampler.Input := LValue.GetValue<Integer>('input');
    LSampler.Output := LValue.GetValue<Integer>('output');
    case AnsiIndexText(LValue.GetValue<string>('interpolation', 'LINEAR'), ['LINEAR', 'STEP', 'CUBICSPLINE']) of
      0: LSampler.Interpolation := iLinear;
      1: LSampler.Interpolation := iStep;
      2: LSampler.Interpolation := iCubicSpline;
    else
      LSampler.Interpolation := iLinear;
    end;
    Result[i] := LSampler;
  end;

end;

class function TGLTFMeshLoader.ReadSkins(const ADoc: TJSONObject): TArray<TSkin>;
var
  LValues: TJSONArray;
  LValue: TJSONObject;
  LSkin: TSkin;
  i: Integer;
begin
  if not ADoc.TryGetValue<TJSONArray>('skins', LValues) then
    Exit(nil);

  SetLength(Result, LValues.Count);
  for i := 0 to High(Result) do
  begin
    LValue := LValues[i] as TJSONObject;
    LSkin.Name := LValue.GetValue<string>('name', '');
    LSkin.InverseBindMatrices := LValue.GetValue<Integer>('inverseBindMatrices', -1);
    LSkin.Joints := LValue.GetValue<TArray<Integer>>('joints');
    Result[i] := LSkin;
  end;
end;

class function TGLTFMeshLoader.ReadTextures(const ADoc: TJSONObject): TArray<TTexture>;
var
  LValues: TJSONArray;
  LValue: TJSONObject;
  i: Integer;
begin
  if not ADoc.TryGetValue<TJSONArray>('textures', LValues) then
    Exit(nil);
  SetLength(Result, LValues.Count);
  for i := 0 to High(Result) do
  begin
    LValue := LValues[i] as TJSONObject;
    Result[i].Source := LValue.GetValue<Integer>('source');
  end;
end;

initialization
  TMeshLoaders.RegisterLoader(TGLTFMeshLoader);

end.
