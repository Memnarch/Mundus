unit Mundus.PixelBuffer;

interface

uses
  Windows,
  Graphics,
  Mundus.Types;

type
  TPixelBuffer = class
  private
    FBitmap: TBitmap;
    FFirstLine: PRGB32Array;
    FLastLine: PRGB32Array;
    FLineLength: NativeInt;
    FBufferSize: NativeInt;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetGraphic: TGraphic;
    function GetCanvas: TCanvas;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Resize(AWidth, AHeight: Integer);
    procedure Clear;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property FirstLine: PRGB32Array read FFirstLine;
    property LineLength: NativeInt read FLineLength;
    property Graphic: TGraphic read GetGraphic;
    property Canvas: TCanvas read GetCanvas;
  end;

implementation

{ TPixelBuffer }

procedure TPixelBuffer.Clear;
begin
  ZeroMemory(FLastLine, FBufferSize);
end;

constructor TPixelBuffer.Create;
begin
  inherited;
  FBitmap := TBitmap.Create();
  FBitmap.PixelFormat := pf32bit;
end;

destructor TPixelBuffer.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TPixelBuffer.GetCanvas: TCanvas;
begin
  Result := FBitmap.Canvas;
end;

function TPixelBuffer.GetGraphic: TGraphic;
begin
  Result := FBitmap;
end;

function TPixelBuffer.GetHeight: Integer;
begin
  Result := FBitmap.Height;
end;

function TPixelBuffer.GetWidth: Integer;
begin
  Result := FBitmap.Width;
end;

procedure TPixelBuffer.Resize(AWidth, AHeight: Integer);
begin
  FBitmap.SetSize(AWidth, AHeight);
  FFirstLine := FBitmap.ScanLine[0];
  FLastLine := FBitmap.ScanLine[FBitmap.Height-1];
  FLineLength := (LongInt(FBitmap.Scanline[1]) - LongInt(FFirstLine)) div SizeOf(TRGB32);
  FBufferSize := (NativeInt(FBitmap.Scanline[0]) - NativeInt(FLastLine)) + FBitmap.Width * SizeOf(TRGB32);
end;

end.
