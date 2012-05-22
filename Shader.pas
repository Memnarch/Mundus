unit Shader;

interface

uses
  Classes, Types, Graphics, ColorTypes, Math3D, SiAuto, SmartInspect;

type

  TPointF = record
    X: Single;
    Y: Single;
  end;

  TShader = class(TObject)
  private
    FPixelBuffer: TBitmap;
    FPixel: TPoint;
    FLineLength: Integer;
    FFirstLine: PRGB32Array;
    FMinX: Cardinal;
    FMinY: Cardinal;
  public
    constructor Create(APixelBuffer: TBitmap);
    procedure Shade8X8Quad(); virtual; abstract;
    procedure ShadeSinglePixel(); virtual; abstract;
    procedure InitTriangle(AVecA, AVecB, AVecC: TVectorClass4D); virtual; abstract;
    property Pixel: TPoint read FPixel write FPixel;
    property LineLength: Integer read FLineLength;
    property FirstLine: PRGB32Array read FFirstLine;
    property MinX: Cardinal read FMinX write FMinX;
    property MinY: Cardinal read FMinY write FMinY;
    property PixelBuffer: TBitmap read FPixelBuffer;
  end;

  function PointF(X, Y: Single): TPointF;

implementation

function PointF(X, Y: Single): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

{ TShader }

constructor TShader.Create(APixelBuffer: TBitmap);
begin
  FPixelBuffer := APixelBuffer;
  FFirstLIne := FPixelBuffer.ScanLine[0];
  FLineLength := (LongInt(FPixelBuffer.Scanline[1]) - LongInt(FFirstLine)) div SizeOf(TRGB32);
end;

end.
