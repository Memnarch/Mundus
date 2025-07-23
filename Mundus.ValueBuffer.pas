unit Mundus.ValueBuffer;

interface

uses
  SysUtils,
  Mundus.Math,
  Mundus.Texture,
  System.TypInfo,
  System.Rtti;

const
  CMaxFields = 8;

type
  TFieldSize = Byte;
  TFieldInfo = record
    Name: string;
    Size: TFieldSize;
    Offset: TFieldSize;
  end;

  PFieldInfo = ^TFieldInfo;

  TFieldInfos = array[0..CMaxFields - 1] of TFieldInfo;

  TValueBufferDescriptor = record
  private
    FFieldCount: Integer;
    FFields: TFieldInfos;
    FRecordSize: NativeUInt;
  public
    class function Create(AInfo: TRttiRecordType): TValueBufferDescriptor; overload; static;
    class function Create<T: record>(): TValueBufferDescriptor; overload; static;
    function TryGetField(const AName: string; out AField: PFieldInfo): Boolean;
    property RecordSize: NativeUInt read FRecordSize;
  end;

  TValueBuffer = record
  private
    FDescriptor: TValueBufferDescriptor;
    FRecordCount: Integer;
    FData: TArray<Byte>;
  public
    procedure Initialize(const ADescriptor: TValueBufferDescriptor; const ARecordCount: Integer);
    procedure BindArray<T: record>(const AName: string; const AValues: TArray<T>); overload;
    procedure BindArray(const AName: string; const AValues: TArray<Single>); overload;
    procedure BindArray(const AName: string; const AValues: TArray<TFloat2>); overload;
    procedure BindArray(const AName: string; const AValues: TArray<TFloat3>); overload;
    procedure BindArray(const AName: string; const AValues: TArray<TFloat4>); overload;
    procedure BindArray(const AName: string; const AValues: TArray<TMatrix4x4>); overload;
    procedure Bind<T: record>(const AName: string; const AValue: T); overload;
    procedure Bind(const AName: string; const AValue: TObject); overload;
    property Data: TArray<Byte> read FData;
    property Descriptor: TValueBufferDescriptor read FDescriptor;
  end;

  PValueBuffer = ^TValueBuffer;

implementation

uses
  System.Math,
  Winapi.Windows;

procedure RaiseToManyFields;
begin
  raise Exception.Create('Attribute record has more than ' + IntToStr(CMaxFields) + ' fields');
end;


{ TValueBufferDescriptor }

class function TValueBufferDescriptor.Create(AInfo: TRttiRecordType): TValueBufferDescriptor;
var
  LFields: TArray<TRttiField>;
  LField: TRttiField;
  i: Integer;
  LFieldInfo: TFieldInfo;
  LOffset: TFieldSize;
begin
  LFields := AInfo.GetFields();
  Result.FFieldCount := Length(LFields);
  Result.FRecordSize := AInfo.TypeSize;

  if Result.FFieldCount > CMaxFields then
    RaiseToManyFields;

  LOffset := 0;
  for i := 0 to High(LFields) do
  begin
    LField := LFields[i];
    LFieldInfo.Name := LField.Name;
    LFieldInfo.Offset := LField.Offset;
    LFieldInfo.Size := LField.FieldType.TypeSize;
    Result.FFields[i] := LFieldInfo;
  end;
end;

class function TValueBufferDescriptor.Create<T>: TValueBufferDescriptor;
var
  LContext: TRttiContext;
begin
  Result := Create(LContext.GetType(TypeInfo(T)) as TRttiRecordType);
end;

function TValueBufferDescriptor.TryGetField(const AName: string; out AField: PFieldInfo): Boolean;
var
  i: Integer;
begin
  for i := 0 to Pred(FFieldCount) do
    if AnsiSameText(AName, FFields[i].Name) then
    begin
      AField := @FFields[i];
      Exit(True);
    end;
  Result := False;
end;

{ TValueBuffer }

procedure TValueBuffer.Bind(const AName: string; const AValue: TObject);
var
  LTarget: ^TObject;
  i: Integer;
  LField: PFieldInfo;
begin
  if FDescriptor.TryGetField(AName, LField) then
  begin
    LTarget := @FData[LField.Offset];
    for i := 0 to Pred(FRecordCount) do
    begin
      LTarget^ := AValue;
      Inc(PByte(LTarget), FDescriptor.FRecordSize);
    end;
  end;
end;

procedure TValueBuffer.Bind<T>(const AName: string; const AValue: T);
var
  LTarget: ^T;
  i: Integer;
  LField: PFieldInfo;
begin
  if FDescriptor.TryGetField(AName, LField) then
  begin
    LTarget := @FData[LField.Offset];
    for i := 0 to Pred(FRecordCount) do
    begin
      LTarget^ := AValue;
      Inc(PByte(LTarget), FDescriptor.FRecordSize);
    end;
  end;
end;

procedure TValueBuffer.BindArray(const AName: string; const AValues: TArray<TFloat2>);
begin
  BindArray<TFloat2>(AName, AValues);
end;

procedure TValueBuffer.BindArray(const AName: string; const AValues: TArray<TFloat3>);
begin
  BindArray<TFloat3>(AName, AValues);
end;

procedure TValueBuffer.BindArray(const AName: string; const AValues: TArray<TFloat4>);
begin
  BindArray<TFloat4>(AName, AValues);
end;

procedure TValueBuffer.BindArray(const AName: string; const AValues: TArray<TMatrix4x4>);
begin
  BindArray<TMatrix4x4>(AName, AValues);
end;

procedure TValueBuffer.BindArray(const AName: string; const AValues: TArray<Single>);
begin
  BindArray<Single>(AName, AValues);
end;

procedure TValueBuffer.BindArray<T>(const AName: string; const AValues: TArray<T>);
var
  LTarget, LSource: ^T;
  i: Integer;
  LField: PFieldInfo;
begin
  if FDescriptor.TryGetField(AName, LField) then
  begin
    LTarget := @FData[LField.Offset];
    LSource := @AValues[0];
    for i := 0 to Pred(FRecordCount) do
    begin
      LTarget^ := LSource^;
      Inc(PByte(LTarget), FDescriptor.FRecordSize);
      Inc(LSource);
    end;
  end;
end;

procedure TValueBuffer.Initialize(const ADescriptor: TValueBufferDescriptor; const ARecordCount: Integer);
var
  LSize: NativeUInt;
begin
  FDescriptor := ADescriptor;
  FRecordCount := ARecordCount;
  LSize := FDescriptor.FRecordSize * ARecordCount;
  if Length(FData) < LSize then
    SetLength(FData, LSize);
end;

end.
