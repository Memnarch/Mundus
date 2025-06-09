unit Mundus.Mesh.Loader.FBX.Types;

interface

uses
  SysUtils,
  System.Rtti,
  Mundus.Types;

type
  ///basic types to parse the document
  TFBXHeader = packed record
    Magic: array[0..20] of AnsiChar;
    Unknown: array[0..1] of Byte;
    Version: UInt32;
  end;

  TNodeHeader74 = packed record
    EndOffset: UInt32;
    NumProperties: UInt32;
    PropertyListLen: UInt32;
    NameLen: Byte;
  end;

  TNodeHeader75 = packed record
    EndOffset: UInt64;
    NumProperties: UInt64;
    PropertyListLen: UInt64;
    NameLen: Byte;
  end;

  TNodeHeader = TNodeHeader75;

  TArrayHeader = packed record
    ArrayLength: UInt32;
    Encoding: UInt32;
    CompressedLength: UInt32;
  end;

  TPropertyType = (ptUnknown, ptInt16, ptBoolean, ptInt32, ptFloat32, ptFloat64, ptInt64, ptString, ptRaw,
    ptArrayFloat32, ptArrayFloat64, ptArrayInt64, ptArrayInt32, ptArrayBoolean);

  TNodeProperty = packed record
  private
    function GetAsBoolean: Boolean;
    function GetAsDouble: Double;
    function GetAsFloat: Single;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
  public
    TypeCode: TPropertyType;
    Data: TValue;
    property AsInteger: Integer read GetAsInteger;
    property AsInt64: Int64 read GetAsInt64;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsFloat: Single read GetAsFloat;
    property AsDouble: Double read GetAsDouble;
  end;

  PNode = ^TNode;

  TNode = packed record
  private
    function GetIsNull: Boolean;
    function GetChild(const AName: string): PNode;
  public
    Header: TNodeHeader;
    Name: string;
    Properties: TArray<TNodeProperty>;
    Childs: TArray<TNode>;
    property Child[const AName: string]: PNode read GetChild;
    property IsNull: Boolean read GetIsNull;
  end;

//types to store and handled loaded information
  TMappingType = (mtUnknown, mtByVertex, mtByPolygon, mtByPolygonVertex, mtByEdge, mtAllSame);
  TReferenceType = (rtUnknown, rtDirect, rtIndexToDirect);

  TUVLayer = record
    Index: Integer;
    UVs: TArray<TUV>;
    UVIndices: TArray<Int32>;
    MappingType: TMappingType;
    ReferenceType: TReferenceType;
  end;

  TMaterialLayer = record
    MappingType: TMappingType;
    ReferenceType: TReferenceType;
    Materials: TArray<Int32>;
  end;

  TNormalLayer = record
    MappingType: TMappingType;
    ReferenceType: TReferenceType;
    Normals: TArray<TVector>;
  end;

  TGeometry = record
    Vertices: TArray<TVector>;
    VertexIndices: TArray<Int32>;
    UVLayers: TArray<TUVLayer>;
    MaterialLayer: TMaterialLayer;
    NormalLayer: TNormalLayer;
  end;

  TIDElement<T> = record
    ID: Int64;
    Element: T;
  end;

  TConnection = record
    Source: Int64;
    Target: Int64;
    Attribute: string;
  end;

  EFBX = class(Exception)

  end;

  EFBXTypeError = class(EFBX)

  end;

function TypeCodeToPropertyType(ACode: AnsiChar): TPropertyType;

function StrToMappingType(const AText: string): TMappingType;
function StrToReferenceType(const AText: string): TReferenceType;

implementation

uses
  StrUtils;

procedure RaiseChildNotFound(const AName: string);
begin
  raise EFBX.Create('Child does not exist: ' + AName);
end;

procedure RaisePropertyConvertError(AFromType, AToType: TPropertyType);
begin
  raise EFBXTypeError.Create('Can not convert property from type ' + IntToSTr(Ord(AFromType)) + ' to type ' + IntToStr(Ord(AToType)));
end;

function TypeCodeToPropertyType(ACode: AnsiChar): TPropertyType;
begin
  case ACode of
    'Y': Result := ptInt16;
    'C': Result := ptBoolean;
    'I': Result := ptInt32;
    'F': Result := ptFloat32;
    'D': Result := ptFloat64;
    'L': Result := ptInt64;
    'S': Result := ptString;
    'R': Result := ptRaw;
    'f': Result := ptArrayFloat32;
    'd': Result := ptArrayFloat64;
    'l': Result := ptArrayInt64;
    'i': Result := ptArrayInt32;
    'b': Result := ptArrayBoolean;
  else
    Result := ptUnknown;
  end;
end;

function StrToMappingType(const AText: string): TMappingType;
begin
  case IndexText(AText, ['ByPolygon', 'ByPolygonVertex', 'ByVertex', 'ByEdge', 'AllSame']) of
    0: Result := mtByPolygon;
    1: Result := mtByPolygonVertex;
    2: Result := mtByVertex;
    3: Result := mtByEdge;
    4: Result := mtAllSame;
  else
    Result := mtUnknown;
  end;
end;

function StrToReferenceType(const AText: string): TReferenceType;
begin
  case IndexText(AText, ['Direct', 'IndexToDirect', 'Index']) of
    0: Result := rtDirect;
    1, 2: Result := rtIndexToDirect;
  else
    Result := rtUnknown;
  end;
end;

{ TNodeProperty }

function TNodeProperty.GetAsBoolean: Boolean;
begin
  Result := Data.AsBoolean;
end;

function TNodeProperty.GetAsDouble: Double;
begin
  case TypeCode of
    ptInt16: Result := Data.AsInteger;
    ptInt32: Result := Data.AsInteger;
    ptFloat32: Result := Data.AsExtended;
    ptFloat64: Result := Data.AsExtended;
    ptInt64: Result := Data.AsInt64;
  else
    RaisePropertyConvertError(TypeCode, ptFloat64);
    Result := 0;//make compiler happy;
  end;
end;

function TNodeProperty.GetAsFloat: Single;
begin
  Result := Data.AsExtended;
end;

function TNodeProperty.GetAsInt64: Int64;
begin
  Result := Data.AsInt64;
end;

function TNodeProperty.GetAsInteger: Integer;
begin
  Result := Data.AsInteger;
end;

{ TNode }

function TNode.GetChild(const AName: string): PNode;
var
  i: Integer;
begin
  for i := 0 to High(Childs) do
    if SameText(Childs[i].Name, AName) then
      Exit(@Childs[i]);
  RaiseChildNotFound(AName);
  Result := nil;//make compiler happy!
end;

function TNode.GetIsNull: Boolean;
begin
  Result := Header.EndOffset = 0;
end;

end.
