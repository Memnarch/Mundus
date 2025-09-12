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
    FUnpackedData: TArray<TRGB32>;
    FWidth: Integer;
    FHeight: Integer;
    FPixelLineLength: NativeInt;
    FFirstPixel: Pointer;
    FDepthBuffer: System.PSingle;
    FLowDepthBuffer: System.PSingle;
    FPixelBufferSize: NativeInt;
    FDepthBufferSize: NativeInt;
    FLowDepthBufferSize: NativeInt;
    FInfo: TBitmapInfo;
  protected
    function PixelSize: Integer; virtual;
    function DepthSize: Integer; virtual;
    function Unpack: PRGB32; virtual;
  public
    constructor Create;
    procedure Resize(AWidth, AHeight: Integer);
    procedure Clear;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property FirstPixel: Pointer read FFirstPixel;
    property PixelLineLength: NativeInt read FPixelLineLength;
    property DepthBuffer: System.PSingle read FDepthBuffer;
    property LowDepthBuffer: System.PSingle read FLowDepthBuffer;
  end;

  TFrameBuffer<TPixelFormat, TDepthFormat: record> = class(TFrameBuffer)
  protected
    function PixelSize: Integer; override;
    function DepthSize: Integer; override;
  end;

implementation

uses
  Mundus.Math;

{ TFrameBuffer }

procedure TFrameBuffer.Clear;
var
  i: Integer;
  LLowDepth: System.PSingle;
begin
  ZeroMemory(@FData[0], FPixelBufferSize + FDepthBufferSize);
  LLowDepth := FLowDepthBuffer;
  for i := 1 to FLowDepthBufferSize div DepthSize do
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

function TFrameBuffer.DepthSize: Integer;
begin
  Result := SizeOf(Single);
end;

procedure TFrameBuffer.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  SetDIBitsToDevice(ACanvas.Handle, Rect.Left, Rect.Top, Rect.Width, Rect.Height, 0, 0, 0, FHeight, Unpack(), FInfo, DIB_RGB_COLORS);
end;

function TFrameBuffer.PixelSize: Integer;
begin
  Result := SizeOf(TFragment);
end;

const
  CAlign = 64;
  CHighAlign = 63;

function Align(APointer: Pointer): Pointer;
var
  LRest: Integer;
begin
  Result := APointer;
  LRest := NativeUInt(Result) mod CAlign;
  Result := Pointer(NativeUInt(Result) + LRest);
end;

procedure TFrameBuffer.Resize(AWidth, AHeight: Integer);
begin
  FPixelBufferSize := AWidth * AHeight * PixelSize + CHighAlign;
  FDepthBufferSize := AWidth * AHeight * DepthSize + CHighAlign;
  FLowDepthBufferSize := ((AWidth + CQuadSize-1) div CQuadSize) * ((AHeight + CQuadSize - 1) div CQuadSize) * DepthSize + CHighAlign;
  SetLength(FData, FPixelBufferSize + FDepthBufferSize + FLowDepthBufferSize);
  FPixelLineLength := AWidth * PixelSize;
  FFirstPixel := Align(@FData[0]);
  FDepthBuffer := Align(@FData[FPixelBufferSize]);
  FLowDepthBuffer := Align(@FData[FPixelBufferSize + FDepthBufferSize]);
  FWidth := AWidth;
  FHeight := AHeight;
  FInfo.bmiHeader.biWidth := FWidth;
  FInfo.bmiHeader.biHeight := -FHeight;
  SetLength(FUnpackedData, FWidth * FHeight);
end;

type
  TFragments = array[0..3] of TFragment;

  PFragments = ^TFragments;

  TRGBS = array[0..3] of TRGB32;

  PRGBS = ^TRGBS;

function TFrameBuffer.Unpack: PRGB32;
var
  i, k, LX, LY, LTilesX, LTilesY: Integer;
  LSource: PFragments;
  LTileStart: PRGB32;
  LTarget: PRGBS;
const
  CDenormalizer: array[0..7] of Single = (255, 255, 255, 255, 255, 255, 255, 255);
  CPicker: array[0..7] of DWord = (0, 4, 1, 5, 2, 3, 6, 7);
begin
  if not Assigned(FUnpackedData) then Exit(nil);
  Result := @FUnpackedData[0];
  asm
    VZEROUPPER
    vmovups ymm0, [CDenormalizer]
    VMOVUPS ymm4, [CPicker]
  end;
  LSource := FFirstPixel;
  LTilesX := FWidth div CQuadSize;
  LTilesY := FHeight div CQuadSize;
  for k := 0 to Pred(LTilesY) do
  begin
    for i := 0 to Pred(LTilesX) do
    begin
      LTileStart := Result;
      Inc(LTileStart, (k*FWidth*CQuadSize + i*CQuadSize));
      for LY := 0 to Pred(CQuadSize) do
      begin
        LTarget := PRGBS(LTileStart);
        for LX := 0 to Pred(CQuadSize div 4) do
        begin
          asm
            mov eax, [LSource]
            vmovups ymm1, [eax]
            vmovups ymm2, [eax + 4*8]
            vmulps ymm1, ymm1, ymm0
            vmulps ymm1, ymm2, ymm0
            VCVTTPS2DQ ymm1, ymm1
            VCVTTPS2DQ ymm2, ymm2
            VPACKUSDW ymm3, ymm1, ymm2
            VPACKUSWB ymm3, ymm3, ymm3
            VPERMD ymm3, ymm4, ymm3
            mov eax, [LTarget]
            VMOVUPS [eax], xmm3
          end;
          Inc(LTarget);
          Inc(LSource);
        end;
        Inc(LTileStart, FWidth);
      end;
    end;
  end;
end;

{ TFrameBuffer<TPixelFormat, TDepthFormat> }

function TFrameBuffer<TPixelFormat, TDepthFormat>.DepthSize: Integer;
begin
  Result := SizeOf(TPixelFormat);
end;

function TFrameBuffer<TPixelFormat, TDepthFormat>.PixelSize: Integer;
begin
  Result := SizeOf(TDepthFormat);
end;

end.
