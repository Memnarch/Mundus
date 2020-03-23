unit Mundus.Texture;

interface

uses
  Classes,
  Types,
  Graphics,
  Mundus.Types,
  Math;

{$IFDEF Debug}
  {$Inline off}
{$ENDIF}

type
  TTexture = class
  private
    FBitmap: TBitmap;
    FFirst: PRGB32Array;
    FLineLengthInPixel: Integer;
    FWidthMask: Integer;
    FHeightMask: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetMaxX: Integer;
    function GetMaxY: Integer;
    function CalculateAndMask(AValue: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const AFile: string);
    procedure Sample(AX, AY: Integer; const ATarget: PRGB32); inline;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property MaxX: Integer read GetMaxX;
    property MaxY: Integer read GetMaxY;
  end;

implementation

uses
  SysUtils;

{ TTexture }

function TTexture.CalculateAndMask(AValue: Integer): Integer;
var
  LValue: Integer;
begin
  Result := 0;
  LValue := AValue;
  while (LValue and 1) = 0 do
  begin
    Result := Result shl 1;
    Inc(Result);
    LValue := LValue shr 1;
  end;
  if LValue > 1 then
    raise Exception.Create('Texture sizes are limited to values with a power of 2 and ' + IntToStr(AValue) + ' is not');
end;

constructor TTexture.Create;
begin
  inherited;
  FBitmap := TBitmap.Create();
end;

destructor TTexture.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TTexture.GetHeight: Integer;
begin
  Result := FBitmap.Height;
end;


function TTexture.GetMaxX: Integer;
begin
  Result := FBitmap.Width - 1;
end;

function TTexture.GetMaxY: Integer;
begin
  Result := FBitmap.Height - 1;
end;

function TTexture.GetWidth: Integer;
begin
  Result := FBitmap.Width;
end;

procedure TTexture.LoadFromFile(const AFile: string);
begin
  FBitmap.LoadFromFile(AFile);
  FBitmap.PixelFormat := pf32bit;
  FFirst := FBitmap.ScanLine[0];
  FLineLengthInPixel := (Longint(FBitmap.Scanline[1]) - Longint(FFirst)) div SizeOf(TRGB32);
  FWidthMask := CalculateAndMask(FBitmap.Width);
  FHeightMask := CalculateAndMask(FBitmap.Height);
end;

procedure TTexture.Sample(AX, AY: Integer; const ATarget: PRGB32);
begin
  ATarget^ := FFirst[(AY and FHeightMask) * FLineLengthInPixel + (AX and FWidthMask)];
end;

end.
