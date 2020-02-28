unit Shader;

interface

uses
  Classes, Types, Graphics, ColorTypes, Math3D;

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
    procedure SetPixelBuffer(const Value: TBitmap);
  public
    constructor Create();
    procedure Shade8X8Quad(); virtual; abstract;
    procedure ShadeSinglePixel(); virtual; abstract;
    procedure InitTriangle(AVecA, AVecB, AVecC: TVectorClass4D); virtual; abstract;
    property Pixel: TPoint read FPixel write FPixel;
    property LineLength: Integer read FLineLength;
    property FirstLine: PRGB32Array read FFirstLine;
    property MinX: Cardinal read FMinX write FMinX;
    property MinY: Cardinal read FMinY write FMinY;
    property PixelBuffer: TBitmap read FPixelBuffer write SetPixelBuffer;
  end;

  function PointF(X, Y: Single): TPointF;

implementation

function PointF(X, Y: Single): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

{ TShader }

constructor TShader.Create();
begin
  
end;

procedure TShader.SetPixelBuffer(const Value: TBitmap);
begin
  FPixelBuffer := Value;
  if Assigned(FPixelBuffer) then
  begin
    FFirstLine := FPixelBuffer.ScanLine[0];
    FLineLength := (LongInt(FPixelBuffer.Scanline[1]) - LongInt(FFirstLine)) div SizeOf(TRGB32);
  end;
end;

end.
