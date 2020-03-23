unit Mundus.ValueBuffer;

interface

uses
  SysUtils,
  Mundus.Math,
  Mundus.Texture;

type
  TValueBinding = type SmallInt;

  TValueBuffer<T> = record
  private
    FValues: array[0..7] of T;
    FNames: array [0..7] of string;
    FCount: Byte;
    function GetValue(Index: TValueBinding): T; inline;
    procedure SetValue(Index: TValueBinding; const Value: T); inline;
  public
    function Bind(const AName: string): TValueBinding;
    function GetBinding(const AName: string): TValueBinding;
    procedure Reset; inline;
    property Values[Index: TValueBinding]: T read GetValue write SetValue; default;
  end;

  TValueBuffers = record
  private
    FMatrix4X4: TValueBuffer<TMatrix4x4>;
    FFloat2Array: TValueBuffer<TArray<TFloat2>>;
    FFloat3Array: TValueBuffer<TArray<TFloat3>>;
    FFloat4ARray: TValueBuffer<TArray<TFloat4>>;
    FFloatArray: TValueBuffer<TArray<Single>>;
    FFloat2: TValueBuffer<TFloat2>;
    FFloat3: TValueBuffer<TFloat3>;
    FFloat4: TValueBuffer<TFloat4>;
    FFloat: TValueBuffer<Single>;
    FTexture: TValueBuffer<TTexture>;
  public
    procedure Reset; inline;
    property Float: TValueBuffer<Single> read FFloat;
    property Float2: TValueBuffer<TFloat2> read FFloat2;
    property Float3: TValueBuffer<TFloat3> read FFloat3;
    property Float4: TValueBuffer<TFloat4> read FFloat4;
    property Matrix4x4: TValueBuffer<TMatrix4x4> read FMatrix4X4;
    property Texture: TValueBuffer<TTexture> read FTexture;
    property FloatArray: TValueBuffer<TArray<Single>> read FFloatArray;
    property Float2Array: TValueBuffer<TArray<TFloat2>> read FFloat2Array;
    property Float3Array: TValueBuffer<TArray<TFloat3>> read FFloat3Array;
    property Float4Array: TValueBuffer<TArray<TFloat4>> read FFloat4ARray;
  end;

  PValueBuffers = ^TValueBuffers;

implementation

{ TValueBuffer<T> }

function TValueBuffer<T>.Bind(const AName: string): TValueBinding;
begin
  FNames[FCount] := AName;
  Result := FCount;
  Inc(FCount);
end;

function TValueBuffer<T>.GetBinding(const AName: string): TValueBinding;
var
  i: Integer;
begin
  for i := 0 to High(FNames) do
    if SameText(FNames[i], AName) then
      Exit(i);
  Result := -1;
end;

function TValueBuffer<T>.GetValue(Index: TValueBinding): T;
begin
  Result := FValues[Index];
end;

procedure TValueBuffer<T>.Reset;
begin
  FCount := 0;
end;

procedure TValueBuffer<T>.SetValue(Index: TValueBinding; const Value: T);
begin
  FValues[Index] := Value;
end;

{ TValueBuffers }

procedure TValueBuffers.Reset;
begin
  FFloat.Reset;
  FFloat2.Reset;
  FFloat3.Reset;
  FFloat4.Reset;
  FMatrix4X4.Reset;
  FTexture.Reset;
  FFloatArray.Reset;
  FFloat2Array.Reset;
  FFloat3Array.Reset;
  FFloat4Array.Reset;
end;

end.
