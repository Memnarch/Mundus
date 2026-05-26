unit Mundus.DrawCall;

interface

uses
  Generics.Collections,
  SysUtils,
  Windows,
  Mundus.Math,
  Mundus.Types,
  Mundus.Shader,
  Mundus.ValueBuffer,
  Mundus.GeometryBuffer;

type
  PSingle = System.PSingle;

  TDrawCall = record
  private
    FGeometry: TGeometryBuffer;
    procedure Reset;
    class function Create: TDrawCall; static;
  public
    property Geometry: TGeometryBuffer read FGeometry;
  end;

  PDrawCall = ^TDrawCall;

  TDrawCalls = class
  private
    FDrawCalls: TArray<TDrawCall>;
    FCallCount: Integer;
    function GetCalls(Index: Integer): PDrawCall;
  public
    function Add: PDrawCall;
    procedure Reset;
    property Calls[Index: Integer]: PDrawCall read GetCalls; default;
    property Count: Integer read FCallCount;
  end;

implementation

const
  CBufferStep  = 1;

{ TDrawCall }

class function TDrawCall.Create: TDrawCall;
begin
  Result := Default(TDrawCall);
  Result.FGeometry := TGeometryBuffer.Create();
end;

procedure TDrawCall.Reset;
begin
  FGeometry.BindShader(nil);
end;

{ TDrawCalls }

function TDrawCalls.Add: PDrawCall;
begin
  if FCallCount = Length(FDrawCalls) then
  begin
    SetLength(FDrawCalls, Length(FDrawCalls) + 1);
    FDrawCalls[FCallCount] := TDrawCall.Create();
  end;
  Result := @FDrawCalls[FCallCount];
  Result.Reset;
  Inc(FCallCount);
end;

function TDrawCalls.GetCalls(Index: Integer): PDrawCall;
begin
  Result := @FDrawCalls[Index];
end;

procedure TDrawCalls.Reset;
begin
  FCallCount := 0;
end;

end.
