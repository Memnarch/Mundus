unit Mundus.FrameBuffer;

interface

uses
  Graphics,
  Winapi.Windows,
  System.Types,
  System.SysUtils,
  Mundus.Types;

type
  TFrameBuffer = class
  private
    FData: TBytes;
    FWidth: Integer;
    FHeight: Integer;
    FPixelLineLength: NativeInt;
    FFirstPixel: PRGB32;
    FDepthBuffer: System.PSingle;
    FLowDepthBuffer: System.PSingle;
    FPixelBufferSize: NativeInt;
    FDepthBufferSize: NativeInt;
    FLowDepthBufferSize: NativeInt;
    FInfo: TBitmapInfo;
  public
    constructor Create;
    procedure Resize(AWidth, AHeight: Integer);
    procedure Clear;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property FirstPixel: PRGB32 read FFirstPixel;
    property PixelLineLength: NativeInt read FPixelLineLength;
    property DepthBuffer: System.PSingle read FDepthBuffer;
    property LowDepthBuffer: System.PSingle read FLowDepthBuffer;
  end;

implementation

type
  TPixelFormat = TRGB32;
  TDepthFormat = Single;

{ TFrameBuffer }

procedure TFrameBuffer.Clear;
var
  i: Integer;
  LLowDepth: System.PSingle;
begin
  ZeroMemory(@FData[0], FPixelBufferSize + FDepthBufferSize);
  LLowDepth := FLowDepthBuffer;
  for i := 1 to FLowDepthBufferSize div SizeOf(TDepthFormat) do
  begin
    LLowDepth^ := 1;
    Inc(LLowDepth);
  end;
end;

constructor TFrameBuffer.Create;
begin
  inherited Create();
  FInfo.bmiHeader.biSize := SizeOf(FInfo.bmiHeader);
  FInfo.bmiHeader.biPlanes := 1;
  FInfo.bmiHeader.biBitCount := 32;
  FInfo.bmiHeader.biCompression := BI_RGB;
  FInfo.bmiHeader.biSizeImage := 0;
end;

procedure TFrameBuffer.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  SetDIBitsToDevice(ACanvas.Handle, Rect.Left, Rect.Top, Rect.Width, Rect.Height, 0, 0, 0, FHeight, @FData[0], FInfo, DIB_RGB_COLORS);
end;

procedure TFrameBuffer.Resize(AWidth, AHeight: Integer);
begin
  FPixelBufferSize := AWidth * AHeight * SizeOf(TPixelFormat);
  FDepthBufferSize := AWidth * AHeight * SizeOf(TDepthFormat);
  FLowDepthBufferSize := ((AWidth + CQuadSize-1) div CQuadSize) * ((AHeight + CQuadSize - 1) div CQuadSize) * SizeOf(TDepthFormat);
  SetLength(FData, FPixelBufferSize + FDepthBufferSize + FLowDepthBufferSize);
  FPixelLineLength := AWidth * SizeOf(TPixelFormat);
  FFirstPixel := @FData[0];
  FDepthBuffer := @FData[FPixelBufferSize];
  FLowDepthBuffer := @FData[FPixelBufferSize + FDepthBufferSize];
  FWidth := AWidth;
  FHeight := AHeight;
  FInfo.bmiHeader.biWidth := FWidth;
  FInfo.bmiHeader.biHeight := -FHeight;
end;

end.
