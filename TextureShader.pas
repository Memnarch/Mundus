unit TextureShader;

interface

uses
  Classes, Types, SysUtils, Shader, Math3D, BaseMesh, Graphics, ColorTypes, XMM;

type
  TTextureShader = class(TShader)
  private
    FUA, FUB, FUC, FUD: Single;
    FVA, FVB, FVC, FVD: Single;
    FZA, FZB, FZC, FZD: Single;
    FAW, FBW, FCW: Single;
    FVecA, FVecB, FVecC: TVectorClass4D;
    FTexHeight, FTexWidth, FTexMaxX, FTexMaxY: Integer;
    FTexture: TBitmap;
    FTexFirstLine: PRGB32Array;
    FTexLineLength: Integer;
    //some filed for XMM register sized structures
    FStepA: TXMMF32;
    FStepB: TXMMF32;
    FStepC: TXMMF32;
    FStepD: TXMMF32;
    //some pointers pointing to the previous declared registers
    FPZUVA: PXMMF32;
    FPZUVB: PXMMF32;
    FPZUVC: PXMMF32;
    FPZUVD: PXMMF32;
  public
    constructor Create();
    procedure InitTriangle(AVecA, AvecB, AvecC: TVectorClass4D); override;
    procedure InitUV(AUVA, AUVB, AUVC: TUV);
    procedure InitTexture(ATexture: TBitmap);
    procedure Shade8X8Quad(); override;
    procedure ShadeSinglePixel(); override;
  end;

implementation

uses
  Interpolation, Math;

{ TTextureShader }

constructor TTextureShader.Create;
begin
  inherited;
  FPZUVA := @FStepA;
  FPZUVB := @FStepB;
  FPZUVC := @FStepC;
  FPZUVD := @FStepD;
end;

procedure TTextureShader.InitTexture(ATexture: TBitmap);
begin
  FTexture := ATexture;
  if not (FTexture.PixelFormat = pf32bit) then raise Exception.Create('wrong pixelformat');
  FTexHeight := FTexture.Height;
  FTexWidth := FTexture.Width;
  FTexMaxX := FTexWidth-1;
  FTexMaxY := FTexHeight-1;
  FTexFirstLine := FTexture.ScanLine[0];
  FTexLineLength := (Longint(FTexture.Scanline[1]) - Longint(FTexFirstLine)) div SizeOf(TRGB32);
end;

procedure TTextureShader.InitTriangle(AVecA, AvecB, AvecC: TVectorClass4D);
begin
  FVecA := AVecA;
  FVecB := AVecB;
  FVecC := AVecC;
  FAW := AVecA.Element[3];
  FBW := AvecB.Element[3];
  FCW := AvecC.Element[3];
  FZC := CalculateFactorC(AVecA, AVecB, AVecC);
  FZA := CalculateFactorA(AVecA, AVecB, AVecC, 1/FAW, 1/FBW, 1/FCW) / FZC;
  FZB := CalculateFactorB(AVecA, AVecB, AVecC, 1/FAW, 1/FBW, 1/FCW) / FZC;
  FZD := CalculateFactorD(AVecA, AVecB, AVecC, 1/FAW, 1/FBW, 1/FCW) / FZC;
end;

procedure TTextureShader.InitUV(AUVA, AUVB, AUVC: TUV);
begin
  FUC := CalculateFactorC(FVecA, FVecB, FVecC);
  FUA := CalculateFactorA(FVecA, FVecB, FVecC, AUVA.U/FAW, AUVB.U/FBW, AUVC.U/FCW) / FUC;
  FUB := CalculateFactorB(FVecA, FVecB, FVecC, AUVA.U/FAW, AUVB.U/FBW, AUVC.U/FCW) / FUC;
  FUD := CalculateFactorD(FVecA, FVecB, FVecC, AUVA.U/FAW, AUVB.U/FBW, AUVC.U/FCW) / FUC;

  FVC := CalculateFactorC(FVecA, FVecB, FVecC);
  FVA := CalculateFactorA(FVecA, FVecB, FVecC, AUVA.V/FAW, AUVB.V/FBW, AUVC.V/FCW) / FVC;
  FVB := CalculateFactorB(FVecA, FVecB, FVecC, AUVA.V/FAW, AUVB.V/FBW, AUVC.V/FCW) / FVC;
  FVD := CalculateFactorD(FVecA, FVecB, FVecC, AUVA.V/FAW, AUVB.V/FBW, AUVC.V/FCW) / FVC;
end;

{$CODEALIGN 16}
procedure TTextureShader.Shade8X8Quad;
var
  LX, LY, LPixelY: Integer;
  LTexX, LTexY: Integer;
  LZ: Double;
  LFUX, LFUY, LFVX, LFVY, LFZX, LFZY: Single;
  LMaxX, LMaxY: Integer;
begin
  LMaxX := FTexMaxX;
  LMaxY := FTexMaxY;
  LFZY := FZB*Pixel.Y + FZD;
  LFUY := FUB*Pixel.Y + FUD;
  LFVY := FVB*Pixel.Y + FVD;
  LPixelY := Pixel.Y*LineLength;
  for LY  := Pixel.Y to Pixel.Y + 7 do
  begin
    LFZX := FZA * Pixel.X + LFZY;
    LFUX := FUA * Pixel.X + LFUY;
    LFVX := FVA * Pixel.X + LFVY;
    for LX := Pixel.X to Pixel.X + 7 do
    begin
      LZ := 1/(LFZX);
//      LTexX := Round(LFUX*LMaxX*LZ);
      asm
        fild LMaxX
        FMul LFUX
        FMul LZ
        FISTP LTexX
        wait
      end;
//      LTexY := Round(LFVX*FTexMaxY*LZ);
      asm
        fild LMaxY
        FMul LFVX
        FMul LZ
        FISTP LTexY
        wait
      end;


      FirstLine[LPixelY + LX] := FTexFirstLine[LTexY*FTexLineLength + LTexX];

      LFZX := LFZX + FZA;
      LFUX := LFUX + FUA;
      LFVX := LFVX + FVA;
    end;
    LFZY := LFZY + FZB;
    LFUY := LFUY + FUB;
    LFVY := LFVY + FVB;
    LPixelY := LPixelY + LineLength;
  end;
end;

{$CODEALIGN 16}
procedure TTextureShader.ShadeSinglePixel;
var
  LX, LY, LPixel, LTexPixel: Integer;
  LTexX, LTexY: Integer;
  LU, LV: Single;
  LZ: Single;
begin
    LY := Pixel.Y;
    LX := Pixel.X;
    LPixel := LY*LineLength + LX;
    LZ := 1/(FZA*LX + FZB * LY + FZD);
    LU := (FUA*LX + FUB * LY + FUD)*LZ;
    LV := (FVA*LX + FVB * LY + FVD)*LZ;
    LTexX := Round(abs(LU*(FTexMaxX)));
    LTexY := Round(abs(LV*(FTexMaxY)));


    LTexPixel := (LTexY mod FTexHeight)*FTexLineLength + (LTexX mod FTexWidth);
    FirstLine[LPixel] := FTexFirstLine[LTexPixel];
end;

end.
