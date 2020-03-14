unit Texture;

interface

uses
  Classes,
  Types,
  Graphics,
  ColorTypes,
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
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetMaxX: Integer;
    function GetMaxY: Integer;
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

{ TTexture }

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


//instead of -1 we use -2 as single precision errors might cause oversampling
//this extra pixel of safespace should protect us
function TTexture.GetMaxX: Integer;
begin
  Result := FBitmap.Width - 2;
end;

function TTexture.GetMaxY: Integer;
begin
  Result := FBitmap.Height - 2;
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
end;

procedure TTexture.Sample(AX, AY: Integer; const ATarget: PRGB32);
begin
  ATarget^ := FFirst[AY*FLineLengthInPixel + AX];
end;

end.
