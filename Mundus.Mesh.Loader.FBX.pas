unit Mundus.Mesh.Loader.FBX;

interface

uses
  System.Types,
  System.Classes,
  System.Rtti,
  Mundus.Math,
  Mundus.Types,
  Mundus.Mesh,
  Mundus.Mesh.Loader,
  Mundus.Material,
  Mundus.Mesh.Loader.FBX.Types;

type
  TFBXMeshLoader = class(TAbstractMeshLoader)
  private type
    TPropertyDescriptor = record
      Name: string;
      Kind: string;
      Unknown: string;
      Unknown2: string;
    end;
  private
    class function ReadHeader(ASource: TStream): TFBXHeader;
    class procedure ReadNode(const ASource: TStream; AIs64Bit: Boolean; var ANode: TNode);
    class procedure ReadProperty(const ASource: TStream; var ATarget: TNodeProperty);
    class function ReadValue<T>(const ASource: TStream): TValue;
    class function ReadRawBuffer(const ASource: TStream): TValue;
    class function ReadString(const ASource: TStream): TValue;
    class function ReadArray<T>(const ASource: TStream): TValue;
    class function LoadGeometry(const ATarget: TMeshGroup; const ANode: TNode): TGeometry;
    class function LoadMaterial(const ANode: TNode): TMaterialElement;
    class function LoadTexture(const ANode: TNode): TIDElement<TTextureReference>;
    class function LoadUVLayer(const ANode: TNode): TUVLayer;
    class function LoadNormalLayer(const ANode: TNode): TNormalLayer;
    class function ReadVertices(const ANode: TNode): TArray<TVector>;
    class function AddPolygons(AMeshByMaterial: TArray<TMesh>; const AMaterialLayer: TMaterialLayer; const AVertices: TArray<TVector>; const AIndices: TArray<Int32>): TArray<Int32>;
    class procedure AddUVs(AMeshByMaterial: TArray<TMesh>; const AMaterialLayer: TMaterialLayer; const AUVLayer: TUVLayer; const APolygonByPolygonVertex: TArray<Int32>);
    class procedure AddNormals(AMeshByMaterial: TArray<TMesh>; const AMaterialLayer: TMaterialLayer; const ANormalLayer: TNormalLayer; const APolygonByPolygonVertex: TArray<Int32>);
    class function ReadPropertyDescriptor(ANode: TNode): TPropertyDescriptor;
    class function ReadPropertyVector(ANode: TNode): TVector;
    class function LoadMaterialLayer(ANode: TNode): TMaterialLayer;
    class function LoadConnection(ANode: TNode): TConnection;
    class procedure ProcessObjects(ATarget: TMeshGroup; const AGeometries: TArray<TGeometry>; const ATextures: TArray<TTextureReference>; const AMaterials: TArray<TMaterial>);
   public
    class function CanLoad(const AFileName: string): Boolean; override;
    class function LoadFromFile(const AFileName: string): TMeshGroup; override;
   end;

implementation

//https://code.blender.org/2013/08/fbx-binary-file-format-specification/

uses
  System.Generics.Collections,
  System.SysUtils,
  System.StrUtils,
  System.ZLib;

function NormalizeIndex(const AValue: Integer): Integer;
begin
  if AValue < 0 then
    Result := not AValue
  else
    Result := AValue;
end;

{ TFBXMeshLoader }

function ReadPolygon(const ASource: TArray<Int32>; AStart: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := AStart to High(ASource) do
  begin
    Inc(Result);
    if ASource[i] < 0 then
      Break;
  end;
end;

class function TFBXMeshLoader.AddPolygons(AMeshByMaterial: TArray<TMesh>; const AMaterialLayer: TMaterialLayer; const AVertices: TArray<TVector>; const AIndices: TArray<Int32>): TArray<Int32>;
var
  i, k, LPolyCount: Integer;
  LTriangle: TTriangle;
  LIndices: TArray<Int32>;
  LPolygonIndex: Integer;
  LTarget: TMesh;
begin
  Result := nil;
  i := 0;
  LPolygonIndex := 0;
  while i < Length(AIndices) do
  begin
    case AMaterialLayer.MappingType of
      mtAllSame: LTarget := AMeshByMaterial[AMaterialLayer.Materials[0]];
      mtByPolygon: LTarget := AMeshByMaterial[AMaterialLayer.Materials[LPolygonIndex]];
    else
      raise EFBX.Create('Unexpected material mapping');
    end;

    LPolyCount := ReadPolygon(AIndices, i);
    SetLength(LIndices, LPolyCount);
    for k := i to Pred(i+LPolyCount) do
    begin
      LIndices[k-i] := LTarget.AddVertice(AVertices[NormalizeIndex(AIndices[k])]);
      Result := Result + [LPolygonIndex];
    end;

    for k := 2 to Pred(LPolyCount) do
    begin
      LTriangle.VertexA := LIndices[0];
      LTriangle.VertexB := LIndices[k-1];
      LTriangle.VertexC := LIndices[k];
      LTarget.AddTriangle(LTriangle);
    end;

    Inc(i, LPolyCount);
    Inc(LPolygonIndex);
  end;
end;

class procedure TFBXMeshLoader.AddNormals(AMeshByMaterial: TArray<TMesh>; const AMaterialLayer: TMaterialLayer; const ANormalLayer: TNormalLayer; const APolygonByPolygonVertex: TArray<Int32>);

  function GetTargetMesh(AIndex: Integer): TMesh;
  begin
    case AMaterialLayer.MappingType of
      mtAllSame: Result := AMeshByMaterial[AMaterialLayer.Materials[0]];
      mtByPolygon: Result := AMeshByMaterial[AMaterialLayer.Materials[APolygonByPolygonVertex[AIndex]]];
    else
      raise EFBX.Create('Unexpected mapping type');
    end;
  end;

var
  LTarget: TMesh;
  i: Integer;
begin
  if Assigned(ANormalLayer.Indices) then
  begin
    for i := 0 to High(ANormalLayer.Indices) do
    begin
      LTarget := GetTargetMesh(i);
      LTarget.AddNormal(ANormalLayer.Normals[ANormalLayer.Indices[i]]);
    end;
  end
  else
  begin
    for i := 0 to High(ANormalLayer.Normals) do
    begin
      LTarget := GetTargetMesh(i);
      LTarget.AddNormal(ANormalLayer.Normals[i]);
    end;
  end;
end;

class procedure TFBXMeshLoader.AddUVs(AMeshByMaterial: TArray<TMesh>; const AMaterialLayer: TMaterialLayer; const AUVLayer: TUVLayer; const APolygonByPolygonVertex: TArray<Int32>);
var
  i, LUVIndex: Integer;
  LTarget: TMesh;
begin
  for i := 0 to High(AUVLayer.UVIndices) do
  begin
    case AMaterialLayer.MappingType of
      mtAllSame: LTarget := AMeshByMaterial[AMaterialLayer.Materials[0]];
      mtByPolygon: LTarget := AMeshByMaterial[AMaterialLayer.Materials[APolygonByPolygonVertex[i]]];
    else
      raise EFBX.Create('Unexpected mapping type');
    end;
    LUVIndex := AUVLayer.UVIndices[i];
    if LUVIndex > -1 then
      LTarget.AddUV(AUVLayer.UVs[LUVIndex], AUVLayer.Index)
    else
      LTarget.AddUV(UV(0, 0), AUVLayer.Index);
  end;
end;

class function TFBXMeshLoader.ReadVertices(const ANode: TNode): TArray<TVector>;
var
  i: Integer;
  LVertex: TVector;
  LValues: TArray<Double>;
begin
  i := 0;

  LValues := ANode.Properties[0].Data.AsType<TArray<Double>>;
  SetLength(Result, Length(LValues) div 3);
  while i < Length(LValues)  do
  begin
    LVertex.X := LValues[i];
    LVertex.Y := LValues[i+1];
    LVertex.Z := LValues[i+2];
    Result[i div 3] := LVertex;
    Inc(i, 3);
  end;
end;

class function TFBXMeshLoader.CanLoad(const AFileName: string): Boolean;
begin
  Result := EndsText('.fbx', AFileName);
end;

class function TFBXMeshLoader.LoadConnection(ANode: TNode): TConnection;
begin
  Result := Default(TConnection);
  Result.Source := ANode.Properties[1].Data.AsInt64;
  Result.Target := ANode.Properties[2].Data.AsInt64;
  if Length(ANode.Properties) > 3 then
    Result.Attribute := ANode.Properties[3].Data.AsString;
end;

class function TFBXMeshLoader.LoadFromFile(const AFileName: string): TMeshGroup;
var
  LSource: TStream;
  LHeader: TFBXHeader;
  LNode: TNode;
  LChild: TNode;
  LNodes: TArray<TNode>;
  LIs64Bit: Boolean;
  LGeometries: TArray<TGeometry>;
  LMaterials: TArray<TMaterial>;
  LIDTexture: TIDElement<TTextureReference>;
  LTextures: TArray<TTextureReference>;
  LTextureIDs: TArray<Int64>;
  LIDMaterial: TMaterialElement;
  LMaterialIDs: TArray<Int64>;
  LConnections: TArray<TConnection>;
  LConnection, LIndirectConnection: TConnection;
  LMaterialIndex, LTextureIndex, LTargetTextureIndex: Integer;
begin
  Result := TMeshGroup.Create();
  LSource := TFileStream.Create(AFileName, fmOpenRead);
  try
    LHeader := ReadHeader(LSource);
    LIs64Bit := LHeader.Version >= 7500;
    repeat
      ReadNode(LSource, LIs64Bit, LNode);
      if not LNode.IsNull then
        LNodes := LNodes + [LNode];
    until LNode.IsNull;

    for LNode in LNodes do
    begin
      if SameText(LNode.Name, 'Objects') then
      begin
        for LChild in LNode.Childs do
        begin
          case IndexText(LChild.Name, ['Geometry', 'Texture', 'Material']) of
            0: LGeometries := LGeometries +  [LoadGeometry(Result, LChild)];
            1:
            begin
              LIDTexture := LoadTexture(LChild);
              LTextures := LTextures + [LIDTexture.Element];
              LTextureIDs := LTextureIDs + [LIDTexture.ID]
            end;
            2:
            begin
              LIDMaterial := LoadMaterial(LChild);
              LMaterials := LMaterials + [LIDMaterial.Element];
              LMaterialIDs := LMaterialIDs + [LIDMaterial.ID]
            end;
          end;
        end;
      end
      else if SameText(LNode.Name, 'Connections') then
      begin
        for LChild in LNode.Childs do
          LConnections := LConnections + [LoadConnection(LChild)];
      end;
    end;

    ProcessObjects(Result, LGeometries, LTextures, LMaterials);
    //Connections describe the relationship between objects in an FBX Model
    //for our purposes, we're only interested in Texture->Material Connections
    //So we simply use twi Arrays, one with IDs of textures and one with IDs of Materials to find out if our current Connection one of it.
    for LConnection in LConnections do
    begin
      LTextureIndex := TArray.IndexOf<Int64>(LTextureIDs, LConnection.Source);
      LMaterialIndex := TArray.IndexOf<Int64>(LMaterialIDs, LConnection.Target);
      if (LTextureIndex > -1) and (LMaterialIndex > -1) then
        Result.Meshes[LMaterialIndex].AddTextureReference(LTextures[LTextureIndex])
      else if LTextureIndex > -1 then
      begin
        //Target was another Texture. So we simply reiterate connections to find all Materials the TargetTexture is connected to
        //and apply our current Texture to those.
        LTargetTextureIndex := TArray.IndexOf<Int64>(LTextureIDs, LConnection.Target);
        if LTargetTextureIndex > -1 then
        begin
          for LIndirectConnection in LConnections do
          begin
            if LIndirectConnection.Source = LConnection.Target then
            begin
              LMaterialIndex := TArray.IndexOf<Int64>(LMaterialIDs, LIndirectConnection.Target);
              if LMaterialIndex > -1 then
              begin
                Result.Meshes[LMaterialIndex].AddTextureReference(LTextures[LTextureIndex]);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    LSource.Free;
  end;
end;

class function TFBXMeshLoader.LoadGeometry(const ATarget: TMeshGroup; const ANode: TNode): TGeometry;
var
  LChild: TNode;
begin
  Result := Default(TGeometry);
  for LChild in ANode.Childs do
  begin
    case IndexText(LChild.Name, ['Vertices', 'PolygonVertexIndex', 'LayerElementUV', 'LayerElementNormal', 'LayerElementMaterial']) of
      0: Result.Vertices := ReadVertices(LChild);
      1: Result.VertexIndices := LChild.Properties[0].Data.AsType<TArray<Int32>>;
      2: Result.UVLayers := Result.UVLayers + [LoadUVLayer(LChild)];
      3: Result.NormalLayer := LoadNormalLayer(LChild);
      4: Result.MaterialLayer := LoadMaterialLayer(LChild);
    end;
  end;
end;

class function TFBXMeshLoader.LoadMaterial(const ANode: TNode): TMaterialElement;
var
  LChild, LPropChild: TNode;
  LDescriptor: TPropertyDescriptor;
  LZero: Integer;
begin
  Result := TMaterialElement.Create();;
  Result.ID := ANode.Properties[0].Data.AsInt64;
  Result.Element.Name := ANode.Properties[1].Data.AsString;
  LZero := Pos(#0, Result.Element.Name);
  if LZero > 0 then
    Result.Element.Name := Copy(Result.Element.Name, 0, LZero-1);
  for LChild in ANode.Childs do
  begin
    if SameText(LChild.Name, 'Properties70') then
    begin
      for LPropChild in LChild.Childs do
      begin
        LDescriptor := ReadPropertyDescriptor(LPropChild);
        case IndexText(LDescriptor.Name, ['AmbientColor', 'DiffuseColor']) of
          0: Result.Element.AmbientColor := ReadPropertyVector(LPropChild);
          1: Result.Element.DiffuseColor := ReadPropertyVector(LPropChild);
        end;
      end;
    end;
  end;
end;

class function TFBXMeshLoader.LoadMaterialLayer(ANode: TNode): TMaterialLayer;
var
  LChild: TNode;
begin
  Result := Default(TMaterialLayer);
  for LChild in ANode.Childs do
  begin
    case IndexText(LChild.Name, ['MappingInformationType', 'ReferenceInformationType', 'Materials']) of
      0: Result.MappingType := StrToMappingType(LChild.Properties[0].Data.AsString);
      1: Result.ReferenceType := StrToReferenceType(LChild.Properties[0].Data.AsString);
      2: Result.Materials := LChild.Properties[0].Data.AsType<TArray<Int32>>;
    end;
  end;
end;

class function TFBXMeshLoader.LoadNormalLayer(const ANode: TNode): TNormalLayer;
var
  LChild: TNode;
  LNormal: TVector;
  LNormals, LNormalsW: TArray<Double>;
  i: Integer;
  LW: Double;
begin
  Result := Default(TNormalLayer);
  for LChild in ANode.Childs do
  begin
    case IndexText(LChild.Name, ['MappingInformationType', 'ReferenceInformationType', 'Normals', 'NormalsW', 'NormalsIndex']) of
      0: Result.MappingType := StrToMappingType(LChild.Properties[0].Data.AsString);
      1: Result.ReferenceType := StrToReferenceType(LChild.Properties[0].Data.AsString);
      2: LNormals := LChild.Properties[0].Data.AsType<TArray<Double>>;
      3: LNormalsW := LChild.Properties[0].Data.AsType<TArray<Double>>;
      4: Result.Indices := LChild.Properties[0].Data.AsType<TArray<Integer>>;
    end;
  end;

  SetLength(Result.Normals, Length(LNormals) div 3);
  for i := 0 to High(Result.Normals) do
  begin
    if Assigned(LNormalsW) then
    begin
      LW := LNormalsW[i];
      LNormal.X := LNormals[i*3] / LW;
      LNormal.Y := LNormals[i*3+1] / LW;
      LNormal.Z := LNormals[i*3+2] / LW;
    end
    else
    begin
      LNormal.X := LNormals[i*3];
      LNormal.Y := LNormals[i*3+1];
      LNormal.Z := LNormals[i*3+2];
    end;
    Result.Normals[i] := LNormal;
  end;
end;

class function TFBXMeshLoader.LoadTexture(const ANode: TNode): TIDElement<TTextureReference>;
var
  LChild: TNode;
begin
  Result := Default(TIDElement<TTextureReference>);
  Result.ID := ANode.Properties[0].Data.AsInt64;
  for LChild in ANode.Childs do
  begin
    case IndexText(LChild.Name, ['TextureName','FileName', 'RelativeFileName', 'Properties70']) of
      0: Result.Element.Name := LChild.Properties[0].Data.AsString;
      1: Result.Element.FileName := LChild.Properties[0].Data.AsString;
    end;
  end;
end;

class function TFBXMeshLoader.LoadUVLayer(const ANode: TNode): TUVLayer;
var
  i: Integer;
  LUVValues: TArray<Double>;
  LNode: TNode;
  LUV: TUV;
begin
  Result := Default(TUVLayer);
  Result.Index := ANode.Properties[0].AsInteger;

  for LNode in ANode.Childs do
  begin
    case IndexText(LNode.Name, ['MappingInformationType', 'ReferenceInformationType', 'UV', 'UVIndex']) of
      0: Result.MappingType := StrToMappingType(LNode.Properties[0].Data.AsString);
      1: Result.ReferenceType := StrToReferenceType(LNode.Properties[0].Data.AsString);
      2: LUVValues := LNode.Properties[0].Data.AsType<TArray<Double>>;
      3: Result.UVIndices := LNode.Properties[0].Data.AsType<TArray<Int32>>;
    end;
  end;

  SetLength(Result.UVs, Length(LUVValues) div 2);
  for i := 0 to Pred(Length(Result.UVs)) do
  begin
    LUV.U := LUVValues[i*2];
    LUV.V := LUVValues[i*2+1];
    Result.UVs[i] := LUV;
  end;
end;

class procedure TFBXMeshLoader.ProcessObjects(ATarget: TMeshGroup;
  const AGeometries: TArray<TGeometry>;
  const ATextures: TArray<TTextureReference>;
  const AMaterials: TArray<TMaterial>);
var
  LMeshByMaterial: TArray<TMesh>;
  i: Integer;
  LGeometry: TGeometry;
  LPolygonIndexByPolygonVertex: TArray<Int32>;
  LUVLayer: TUVLayer;
begin
  SetLength(LMeshByMaterial, Length(AMaterials));
  for i := 0 to High(AMaterials) do
  begin
    LMeshByMaterial[i] := TMesh.Create();
    ATarget.Meshes.Add(LMeshByMaterial[i]);
    LMeshByMaterial[i].Material := AMaterials[i];
  end;

  for LGeometry in AGeometries do
  begin
    LPolygonIndexByPolygonVertex := AddPolygons(LMeshByMaterial, LGeometry.MaterialLayer, LGeometry.Vertices, LGeometry.VertexIndices);
    AddNormals(LMeshByMaterial, LGeometry.MaterialLayer, LGeometry.NormalLayer, LPolygonIndexByPolygonVertex);
    for LUVLayer in LGeometry.UVLayers do
      AddUVs(LMeshByMaterial, LGeometry.MaterialLayer, LUVLayer, LPolygonIndexByPolygonVertex);
  end;
end;

class function TFBXMeshLoader.ReadArray<T>(const ASource: TStream): TValue;
var
  LValue: TArray<T>;
  LHeader: TArrayHeader;
  LDecompression: TZDecompressionStream;
begin
  ASource.Read(LHeader, SizeOf(LHeader));
  SetLength(LValue, LHeader.ArrayLength);
  case LHeader.Encoding of
    0://Uncompressed
    begin
      ASource.Read(LValue[0], LHeader.ArrayLength * SizeOf(T));
    end;
    1://ZLib compressed
    begin
      LDecompression := TZDecompressionStream.Create(ASource);
      try
        LDecompression.Read(LValue[0], LHeader.ArrayLength * SizeOf(T));
      finally
        LDecompression.Free;
      end;
    end;
  else
    raise EFBX.Create('Unsupported Array encoding: ' + IntToStr(LHeader.Encoding));
  end;

  Result := TValue.From(LValue);
end;

class function TFBXMeshLoader.ReadHeader(ASource: TStream): TFBXHeader;
begin
  ASource.Read(Result, SizeOf(Result));
end;

class procedure TFBXMeshLoader.ReadNode(const ASource: TStream; AIs64Bit: Boolean; var ANode: TNode);
var
  i: UInt64;
  LChild: TNode;
  LHeader74: TNodeHeader74;
  LName: AnsiString;
begin
  ANode := Default(TNode);
  if AIs64Bit then
    ASource.Read(ANode.Header, SizeOf(ANode.Header))
  else
  begin
    ASource.Read(LHeader74, SizeOf(LHeader74));
    ANode.Header.EndOffset := LHeader74.EndOffset;
    ANode.Header.NumProperties := LHeader74.NumProperties;
    ANode.Header.PropertyListLen := LHeader74.PropertyListLen;
    ANode.Header.NameLen := LHeader74.NameLen;
  end;
  if ANode.Header.NameLen > 0 then
  begin
    SetLength(LName, ANode.Header.NameLen);
    ASource.Read(LName[1], ANode.Header.NameLen);
    ANode.Name := string(LName);
  end;

  SetLength(ANode.Properties, ANode.Header.NumProperties);

  if ANode.Header.NumProperties > 0 then
  for i := 0 to Pred(ANode.Header.NumProperties) do
    ReadProperty(ASource, ANode.Properties[i]);

  if ASource.Position < ANode.Header.EndOffset then
  begin
    repeat
      ReadNode(ASource, AIs64Bit, LChild);
      if not LChild.IsNull then
        ANode.Childs := ANode.Childs + [LChild];
    until LChild.IsNull;
  end;
end;

class procedure TFBXMeshLoader.ReadProperty(const ASource: TStream; var ATarget: TNodeProperty);
var
  LCode: AnsiChar;
begin
  ATarget := Default(TNodeProperty);
  ASource.Read(LCode, SizeOf(LCode));
  ATarget.TypeCode := TypeCodeToPropertyType(LCode);
  case ATarget.TypeCode of
    ptInt16: ATarget.Data := ReadValue<Int16>(ASource);
    ptBoolean: ATarget.Data := ReadValue<Byte>(ASource);
    ptInt32: ATarget.Data := ReadValue<Int32>(ASource);
    ptFloat32: ATarget.Data := ReadValue<Single>(ASource);
    ptFloat64: ATarget.Data := ReadValue<Double>(ASource);
    ptInt64: ATarget.Data := ReadValue<Int64>(ASource);
    ptString: ATarget.Data := ReadString(ASource);
    ptRaw: ATarget.Data := ReadRawBuffer(ASource);
    ptArrayFloat32: ATarget.Data := ReadArray<Single>(ASource);
    ptArrayFloat64: ATarget.Data := ReadArray<Double>(ASource);
    ptArrayInt64: ATarget.Data := ReadArray<Int64>(ASource);
    ptArrayInt32: ATarget.Data := ReadArray<Int32>(ASource);
    ptArrayBoolean: ATarget.Data := ReadArray<Byte>(ASource);
  else
    raise EFBX.Create('Unsupported TypeCode ' + LCode);
  end;
end;

class function TFBXMeshLoader.ReadPropertyDescriptor(ANode: TNode): TPropertyDescriptor;
begin
  if Length(ANode.Properties) > 3 then
  begin
    Result.Name := ANode.Properties[0].Data.AsString;
    Result.Kind := ANode.Properties[1].Data.AsString;
    Result.Unknown := ANode.Properties[2].Data.AsString;
    Result.Unknown2 := ANode.Properties[3].Data.AsString;
  end
  else
    Result := Default(TPropertyDescriptor);
end;

class function TFBXMeshLoader.ReadPropertyVector(ANode: TNode): TVector;
begin
  if Length(ANode.Properties) > 6 then
  begin
    Result.X := ANode.Properties[4].Data.AsExtended;
    Result.Y := ANode.Properties[5].Data.AsExtended;
    Result.Z := ANode.Properties[6].Data.AsExtended;
  end;
end;

class function TFBXMeshLoader.ReadRawBuffer(const ASource: TStream): TValue;
var
  LLength: UInt32;
  LBuffer: TBytes;
begin
  ASource.Read(LLength, SizeOf(LLength));
  SetLength(LBuffer, LLength);
  ASource.Read(LBuffer[0], LLength);
  Result := TValue.From(LBuffer);
end;

class function TFBXMeshLoader.ReadString(const ASource: TStream): TValue;
var
  LLength: UInt32;
  LBuffer: AnsiString;
begin
  ASource.Read(LLength, SizeOf(LLength));
  SetLength(LBuffer, LLength);
  if LLength > 0 then
    ASource.Read(LBuffer[1], LLength);
  Result := TValue.From(string(LBuffer));
end;

class function TFBXMeshLoader.ReadValue<T>(const ASource: TStream): TValue;
var
  LValue: T;
begin
  ASource.Read(LValue, SizeOf(LValue));
  Result := TValue.From(LValue);
end;

initialization
  TMeshLoaders.RegisterLoader(TFBXMeshLoader);

end.
