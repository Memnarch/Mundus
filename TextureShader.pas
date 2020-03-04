unit TextureShader;

interface

uses
  Classes, Types, SysUtils, Shader, Math3D, BaseMesh, Graphics, ColorTypes;

type
  TTextureShader = class(TShader)
  private
//    FUA, FUB, FUC, FUD: Single;
//    FVA, FVB, FVC, FVD: Single;
//    FZA, FZB, FZC, FZD: Single;
//    FAW, FBW, FCW: Single;
    FVecA, FVecB, FVecC: TFloat4;
    FTexHeight, FTexWidth, FTexMaxX, FTexMaxY: Integer;
    FTexture: TBitmap;
    FTexFirstLine: PRGB32Array;
    FTexLineLength: Integer;

    FStepA, FStepB, FStepC, FStepD: TFloat4;
  public
    constructor Create();
    procedure InitTriangle(AVecA, AvecB, AvecC: TFloat4); override;
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

procedure TTextureShader.InitTriangle(AVecA, AvecB, AvecC: TFloat4);
begin
  FVecA := AVecA;
  FVecB := AVecB;
  FVecC := AVecC;
  FStepA.W := AVecA.Element[3];
  FStepB.W := AvecB.Element[3];
  FStepC.W := AvecC.Element[3];
  FStepC.Z := CalculateFactorC(AVecA, AVecB, AVecC);
  FStepA.Z := CalculateFactorA(AVecA, AVecB, AVecC, 1/FStepA.W, 1/FStepB.W, 1/FStepC.W) / FStepC.Z;
  FStepB.Z := CalculateFactorB(AVecA, AVecB, AVecC, 1/FStepA.W, 1/FStepB.W, 1/FStepC.W) / FStepC.Z;
  FStepD.Z := CalculateFactorD(AVecA, AVecB, AVecC, 1/FStepA.W, 1/FStepB.W, 1/FStepC.W) / FStepC.Z;
end;

procedure TTextureShader.InitUV(AUVA, AUVB, AUVC: TUV);
begin
  FStepC.XY.U := CalculateFactorC(FVecA, FVecB, FVecC);
  FStepA.XY.U := CalculateFactorA(FVecA, FVecB, FVecC, AUVA.U/FStepA.W, AUVB.U/FStepB.W, AUVC.U/FStepC.W) / FStepC.XY.U;
  FStepB.XY.U := CalculateFactorB(FVecA, FVecB, FVecC, AUVA.U/FStepA.W, AUVB.U/FStepB.W, AUVC.U/FStepC.W) / FStepC.XY.U;
  FStepD.XY.U := CalculateFactorD(FVecA, FVecB, FVecC, AUVA.U/FStepA.W, AUVB.U/FStepB.W, AUVC.U/FStepC.W) / FStepC.XY.U;

  FStepC.XY.V := CalculateFactorC(FVecA, FVecB, FVecC);
  FStepA.XY.V := CalculateFactorA(FVecA, FVecB, FVecC, AUVA.V/FStepA.W, AUVB.V/FStepB.W, AUVC.V/FStepC.W) / FStepC.XY.V;
  FStepB.XY.V := CalculateFactorB(FVecA, FVecB, FVecC, AUVA.V/FStepA.W, AUVB.V/FStepB.W, AUVC.V/FStepC.W) / FStepC.XY.V;
  FStepD.XY.V := CalculateFactorD(FVecA, FVecB, FVecC, AUVA.V/FStepA.W, AUVB.V/FStepB.W, AUVC.V/FStepC.W) / FStepC.XY.V;
end;

{$CODEALIGN 16}
procedure TTextureShader.Shade8X8Quad;
var
  LX, LY, LPixelY: Integer;
  LTexX, LTexY: Integer;
  LZ: Double;
//  LFUX, LFUY, LFVX, LFVY, LFZX, LFZY: Single;
  LMaxX, LMaxY: Integer;
  LFY, LFX: TFloat4;
  LTest: TRGB32;
  LFUV: TFloat2;
begin
  LTest := Default(trgb32);
  LTest.B := 255;
  LMaxX := FTexMaxX;
  LMaxY := FTexMaxY;
//  LFZY := FZB*Pixel.Y + FZD;
//  LFUY := FUB*Pixel.Y + FUD;
//  LFVY := FVB*Pixel.Y + FVD;
  LFY := FStepB;
  LFY.Mul(Pixel.Y);
  LFY.Add(FStepD);
  LPixelY := Pixel.Y*LineLength;
  for LY  := Pixel.Y to Pixel.Y + 7 do
  begin
//    LFZX := FZA * Pixel.X + LFZY;
//    LFUX := FUA * Pixel.X + LFUY;
//    LFVX := FVA * Pixel.X + LFVY;
    LFX := FStepA;
    LFX.Mul(Pixel.X);
    LFX.Add(LFY);
    for LX := Pixel.X to Pixel.X + 7 do
    begin
//      LZ := 1/(LFZX);
      LZ := 1/LFX.Z;
//      LTexX := Round(LFUX*LMaxX*LZ);
      LFUV := LFX.XY;
      LTexX := Round(LFX.XY.U*LMaxX*LZ);
//      asm
//        fild LMaxX
//        FMul LFUX
//        FMul LZ
//        FISTP LTexX
//        wait
//      end;
//      LTexY := Round(LFVX*FTexMaxY*LZ);
        LTexY := Round(LFX.XY.V*LMaxY*LZ);
//      asm
//        fild LMaxY
//        FMul LFVX
//        FMul LZ
//        FISTP LTexY
//        wait
//      end;


      FirstLine[LPixelY + LX] := FTexFirstLine[LTexY*FTexLineLength + LTexX];

      LFX.Add(FStepA);
//      LFZX := LFZX + FZA;
//      LFUX := LFUX + FUA;
//      LFVX := LFVX + FVA;
    end;
      LFY.Add(FStepB);
//    LFZY := LFZY + FZB;
//    LFUY := LFUY + FUB;
//    LFVY := LFVY + FVB;
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
    LZ := 1/(FStepA.Z * LX + FStepB.Z * LY + FStepD.Z);
    LU := (FStepA.XY.U * LX + FStepB.XY.U * LY + FStepD.XY.U)*LZ;
    LV := (FStepA.XY.V * LX + FStepB.XY.V * LY + FStepD.XY.V)*LZ;
    LTexX := Round(abs(LU*(FTexMaxX)));
    LTexY := Round(abs(LV*(FTexMaxY)));


    LTexPixel := (LTexY mod FTexHeight)*FTexLineLength + (LTexX mod FTexWidth);
    FirstLine[LPixel] := FTexFirstLine[LTexPixel];
end;

end.
