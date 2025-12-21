unit Mundus.Mesh.Loader.Gltf;

interface

uses
  Mundus.Mesh.Loader,
  Mundus.Mesh,
  System.JSON,
  Mundus.Math,
  Mundus.Types,
  Mundus.Material,
  Mundus.Mesh.Loader.Gltf.Types;

type
  TGLTFMeshLoader = class(TAbstractMeshLoader)
  protected
    class function ReadBuffers(const ADoc: TJSONObject; const ADirectory: string): TArray<TBuffer>;
    class function ReadBufferViews(const ADoc: TJSONObject): TArray<TBufferView>;
    class function ReadAccessors(const ADoc: TJSONObject): TArray<TAccessor>;
    class function ReadTextures(const ADoc: TJSONObject): TArray<TTexture>;
    class function ReadImages(const ADoc: TJSONObject): TArray<TImage>;
    class function ReadMaterials(const ADoc: TJSONObject): TArray<TMaterial>;
    class function Read<T>(const AData: TGLTFData; const AAccessor: Integer): TArray<T>; overload;
    class procedure ReadMeshes(const ADoc: TJSONObject; const AData: TGLTFData; const ATarget: TMeshGroup);
  public
    class function CanLoad(const AFileName: string): Boolean; override;
    class function LoadFromFile(const AFileName: string): TMeshGroup; override;
  end;

function ElementCount(AType: TElementType): Integer;
function ComponentSize(AType: TComponentType): Integer;

implementation

uses
  System.Generics.Collections,//make compiler happy for inline
  System.StrUtils,
  System.IOUtils,
  System.SysUtils,
  Winapi.Windows;
const
  CMeterToCM = 100;

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

{ TGLTFMeshLoader }

class function TGLTFMeshLoader.CanLoad(const AFileName: string): Boolean;
begin
  Result := EndsText('.gltf', AFileName);
end;

class function TGLTFMeshLoader.LoadFromFile(const AFileName: string): TMeshGroup;
var
  LDocument: TJSONObject;
  LData: TGLTFData;
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
      ReadMeshes(LDocument, LData, Result);
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
