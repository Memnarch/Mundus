unit TextureShader;

interface

uses
  Classes, Types, SysUtils, Shader, Math3D, BaseMesh, Graphics, ColorTypes;

type
  TTextureShader = class(TShader)
  private
    FUA, FUB, FUC, FUD: Single;
    FVA, FVB, FVC, FVD: Single;
    FZA, FZB, FZC, FZD: Single;
    FAW, FBW, FCW: Single;
    FVecA, FVecB, FVecC: TVectorClass4D;
    FTexHeight, FTexWidth: Word;
    FTexture: TBitmap;
    FTexFirstLine: PRGB32Array;
    FTexLineLength: Integer;
  public
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

procedure TTextureShader.InitTexture(ATexture: TBitmap);
begin
  FTexture := ATexture;
  if not (FTexture.PixelFormat = pf32bit) then raise Exception.Create('wrong pixelformat');
  FTexHeight := FTexture.Height;
  FTexWidth := FTexture.Width;
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

procedure TTextureShader.Shade8X8Quad;
var
  LX, LY, LPixel, LTexPixel: Integer;
  LTexX, LTexY: Integer;
  LU, LV: Single;
  LZ: Single;
begin
  for LY  := Pixel.Y to Pixel.Y + 7 do
  begin
    for LX := Pixel.X to Pixel.X + 7 do
    begin
      LPixel := LY*LineLength + LX;
      LZ := 1/(FZA  *LX + FZB * LY + FZD);
      LU := (FUA * LX + FUB * LY + FUD)*LZ;
      LV := (FVA * LX + FVB * LY + FVD)*LZ;
      LTexX := Trunc(abs(LU*(FTexture.Width-1)));
      LTexY := Trunc(abs(LV*(FTexture.Height-1)));

      LTexPixel := (LTexY mod FTexHeight)*FTexLineLength + (LTexX mod FTexWidth);
      FirstLine[LPixel].B := FTexFirstLine[LTexPixel].B;
      FirstLine[LPixel].G := FTexFirstLine[LTexPixel].G;
      FirstLine[LPixel].R := FTexFirstLine[LTexPixel].R;
    end;
  end;
end;

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
    LTexX := Trunc(abs(LU*(FTexture.Width-1)));
    LTexY := Trunc(abs(LV*(FTexture.Height-1)));


    LTexPixel := (LTexY mod FTexHeight)*FTexLineLength + (LTexX mod FTexWidth);
    FirstLine[LPixel].B := FTexFirstLine[LTexPixel].B;
    FirstLine[LPixel].G := FTexFirstLine[LTexPixel].G;
    FirstLine[LPixel].R := FTexFirstLine[LTexPixel].R;
end;

end.
