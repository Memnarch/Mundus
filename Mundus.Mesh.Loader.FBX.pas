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
  private
    class function ReadHeader(ASource: TStream): TFBXHeader;
    class procedure ReadNode(const ASource: TStream; var ANode: TNode);
    class procedure ReadProperty(const ASource: TStream; var ATarget: TNodeProperty);
    class function ReadValue<T>(const ASource: TStream): TValue;
    class function ReadRawBuffer(const ASource: TStream): TValue;
    class function ReadString(const ASource: TStream): TValue;
    class function ReadArray<T>(const ASource: TStream): TValue;
    class function LoadGeometry(const ATarget: TMeshGroup; const ANode: TNode): TMesh;
    class function ReadVertices(const ANode: TNode): TArray<TVector>;
    class procedure AddIndices(ATarget: TMesh; const ANode: TNode; const AVertices: TArray<TVector>);
    class procedure AddUVs(ATarget: TMesh; const ANode: TNode);
   public
    class function CanLoad(const AFileName: string): Boolean; override;
    class function LoadFromFile(const AFileName: string): TMeshGroup; override;
   end;

implementation

//https://code.blender.org/2013/08/fbx-binary-file-format-specification/

uses
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

class procedure TFBXMeshLoader.AddIndices(ATarget: TMesh; const ANode: TNode; const AVertices: TArray<TVector>);
var
  i, k, LPolyCount: Integer;
  LTriangle: TTriangle;
  LValues: TArray<Int32>;
  LIndices: TArray<Int32>;
begin
  LValues := ANode.Properties[0].Data.AsType<TArray<Int32>>;
  i := 0;
  while i < Length(LValues) do
  begin
    LPolyCount := ReadPolygon(LValues, i);
    SetLength(LIndices, LPolyCount);
    for k := i to Pred(i+LPolyCount) do
      LIndices[k-i] := ATarget.AddVertice(AVertices[NormalizeIndex(LValues[k])]);
    if LPolyCount > 2 then
    begin
      LTriangle.VertexA := LIndices[0];
      LTriangle.VertexB := LIndices[1];
      LTriangle.VertexC := LIndices[2];
      ATarget.AddTriangle(LTriangle);
    end;
    Inc(i, LPolyCount);
  end;
end;

class procedure TFBXMeshLoader.AddUVs(ATarget: TMesh; const ANode: TNode);
var
  i: Integer;
  LUVValues: TArray<Double>;
  LIndices: TArray<Int32>;
  LNode: TNode;
  LUV: TFloat2;
  LLayerIndex: Int32;
begin
  LLayerIndex := ANode.Properties[0].AsInteger;

  for LNode in ANode.Childs do
  begin
    case IndexText(LNode.Name, ['UV', 'UVIndex']) of
      0: LUVValues := LNode.Properties[0].Data.AsType<TArray<Double>>;
      1: LIndices := LNode.Properties[0].Data.AsType<TArray<Int32>>;
    end;
  end;

  for i := 0 to High(LIndices) do
  begin
    LUV.X := LUVValues[LIndices[i]*2];
    LUV.Y := LUVValues[LIndices[i]*2+1];
    ATarget.AddUV(LUV, LLayerIndex);
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
    //assume the default of Y-Up in FBX and convert to Z-Up
    LVertex.Z := -LValues[i+1];
    LVertex.Y := LValues[i+2];
    Result[i div 3] := LVertex;
    Inc(i, 3);
  end;
end;

class function TFBXMeshLoader.CanLoad(const AFileName: string): Boolean;
begin
  Result := EndsText('.fbx', AFileName);
end;

class function TFBXMeshLoader.LoadFromFile(const AFileName: string): TMeshGroup;
var
  LSource: TStream;
  LHeader: TFBXHeader;
  LNode: TNode;
  LChild: TNode;
  LNodes: TArray<TNode>;
begin
  Result := TMeshGroup.Create();
  LSource := TFileStream.Create(AFileName, fmOpenRead);
  try
    LHeader := ReadHeader(LSource);
    repeat
      ReadNode(LSource, LNode);
      if not LNode.IsNull then
        LNodes := LNodes + [LNode];
    until LNode.IsNull;

    for LNode in LNodes do
    begin
      if SameText(LNode.Name, 'Objects') then
      begin
        for LChild in LNode.Childs do
        begin
          if SameText(LChild.Name, 'Geometry') then
            LoadGeometry(Result, LChild);
        end;
      end;
    end;
  finally
    LSource.Free;
  end;
end;

class function TFBXMeshLoader.LoadGeometry(const ATarget: TMeshGroup; const ANode: TNode): TMesh;
var
  LMesh: TMesh;
  LChild: TNode;
  LVertices: TArray<TVector>;
begin
  LMesh := TMesh.Create();
  try
    for LChild in ANode.Childs do
    begin
      case IndexText(LChild.Name, ['Vertices', 'PolygonVertexIndex', 'LayerElementUV']) of
        0: LVertices := ReadVertices(LChild);
        1: AddIndices(LMesh, LChild, LVertices);
        2: AddUVs(LMesh, LChild);
      end;
    end;
    Result := LMesh;
    LMesh := nil;
  finally
    LMesh.Free;
  end;
  ATarget.Meshes.Add(Result);
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

class procedure TFBXMeshLoader.ReadNode(const ASource: TStream; var ANode: TNode);
var
  i: Integer;
  LChild: TNode;
  LName: AnsiString;
begin
  ANode := Default(TNode);
  ASource.Read(ANode.Header, SizeOf(ANode.Header));
  if ANode.Header.NameLen > 0 then
  begin
    SetLength(LName, ANode.Header.NameLen);
    ASource.Read(LName[1], ANode.Header.NameLen);
    ANode.Name := string(LName);
  end;

  SetLength(ANode.Properties, ANode.Header.NumProperties);
  for i := 0 to Pred(ANode.Header.NumProperties) do
    ReadProperty(ASource, ANode.Properties[i]);
  if ASource.Position < ANode.Header.EndOffset then
  begin
    repeat
      ReadNode(ASource, LChild);
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
