unit TextureShader;

interface

uses
  Classes, Types, SysUtils, Shader, Math3D, BaseMesh, Graphics, ColorTypes, SiAuto, SmartInspect;

type
  TTextureShader = class(TShader)
  private
    FUA, FUB, FUC, FUD: Single;
    FVA, FVB, FVC, FVD: Single;
    FZA, FZB, FZC, FZD: Single;
    FAW, FBW, FCW: Single;
    FVecA, FVecB, FVecC: TVectorClass4D;
    FTexture: TBitmap;
    FTexFirstLine: PRGB32Array;
    FTexLineLength: Cardinal;
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
  FTexFirstLine := FTexture.ScanLine[0];
  FTexLineLength := (Longint(FTexture.Scanline[1]) - Longint(FTexFirstLine)) div SizeOf(TRGB32);
end;

procedure TTextureShader.InitTriangle(AVecA, AvecB, AvecC: TVectorClass4D);
begin
  FVecA := TVectorClass4D.Create();
  FVecA.CopyFromVector4D( AVecA);
  FVecB := TVectorClass4D.Create();
  FVecB.CopyFromVector4D(AvecB);
  FVecC := TVectorClass4D.Create();
  FVecC.CopyFromVector4D(AVecC);
  FAW := AVecA.Element[3];
  FBW := AvecB.Element[3];
  FCW := AvecC.Element[3];
  FZA := CalculateFactorA(AVecA, AVecB, AVecC, 1/FAW, 1/FBW, 1/FCW);
  FZB := CalculateFactorB(AVecA, AVecB, AVecC, 1/FAW, 1/FBW, 1/FCW);
  FZC := CalculateFactorC(AVecA, AVecB, AVecC);
  FZD := CalculateFactorD(AVecA, AVecB, AVecC, 1/FAW, 1/FBW, 1/FCW);
  SiMain.LogSingle('AW', FAW);
  SiMain.LogSingle('BW', FBW);
  SiMain.LogSingle('CW', FCW);
end;

procedure TTextureShader.InitUV(AUVA, AUVB, AUVC: TUV);
begin
  FUA := CalculateFactorA(FVecA, FVecB, FVecC, AUVA.U/FAW, AUVB.U/FBW, AUVC.U/FCW);
  FUB := CalculateFactorB(FVecA, FVecB, FVecC, AUVA.U/FAW, AUVB.U/FBW, AUVC.U/FCW);
  FUC := CalculateFactorC(FVecA, FVecB, FVecC);
  FUD := CalculateFactorD(FVecA, FVecB, FVecC, AUVA.U/FAW, AUVB.U/FBW, AUVC.U/FCW);

  FVA := CalculateFactorA(FVecA, FVecB, FVecC, AUVA.V/FAW, AUVB.V/FBW, AUVC.V/FCW);
  FVB := CalculateFactorB(FVecA, FVecB, FVecC, AUVA.V/FAW, AUVB.V/FBW, AUVC.V/FCW);
  FVC := CalculateFactorC(FVecA, FVecB, FVecC);
  FVD := CalculateFactorD(FVecA, FVecB, FVecC, AUVA.V/FAW, AUVB.V/FBW, AUVC.V/FCW);
end;

procedure TTextureShader.Shade8X8Quad;
var
  LX, LY, LPixel, LTexPixel: Cardinal;
  LTexX, LTexY: Cardinal;
  LU, LV: Single;
  LZ: Single;
begin
  for LY  := Pixel.Y to Pixel.Y + 7 do
  begin
    for LX := Pixel.X to Pixel.X + 7 do
    begin
      LPixel := LY*LineLength + LX;
      LZ := (FZA  *LX + FZB * LY + FZD) / FZC;
      LZ := 1 / LZ;
      LU := (FUA * LX + FUB * LY + FUD) / FUC;
      LV := (FVA * LX + FVB * LY + FVD) / FVC;
      LTexX := Trunc(LU*LZ*(FTexture.Width-1));
      LTexY := Trunc(LV*LZ*(FTexture.Height-1));

      try
        LTexPixel := (LTexY mod 32)*FTexLineLength + (LTexX mod 32);
        //LValue := Trunc(-(FStepA * LX + FStepB * LY + FStepD)/FStepC);
        FirstLine[LPixel].B := FTexFirstLine[LTexPixel].B;
        FirstLine[LPixel].G := FTexFirstLine[LTexPixel].G;
        FirstLine[LPixel].R := FTexFirstLine[LTexPixel].R;
      except
//        SiMain.LogCardinal('LU', LU);
//        SiMain.LogCardinal('LV', LV);
//        SiMain.LogSingle('LZ', LZ);
      end;
    end;
  end;
end;

procedure TTextureShader.ShadeSinglePixel;
var
  LX, LY, LPixel, LTexPixel: Cardinal;
  LTexX, LTexY: Cardinal;
  LU, LV: Single;
  LZ: Single;
begin
    LY := Pixel.Y;
    LX := Pixel.X;
    LPixel := LY*LineLength + LX;
    LZ := (FZA*LX + FZB * LY + FZD)/FZC;
    LZ := 1/LZ;
    LU := (FUA*LX + FUB * LY + FUD)/FUC;
    LV := (FVA*LX + FVB * LY + FVD)/FVC;
    LTexX := Trunc(LU*LZ*(FTexture.Width-1));
    LTexY := Trunc(LV*LZ*(FTexture.Height-1));

    try
      LTexPixel := (LTexY mod 32)*FTexLineLength + (LTexX mod 32);
      //LValue := Trunc(-(FStepA * LX + FStepB * LY + FStepD)/FStepC);
      FirstLine[LPixel].B := FTexFirstLine[LTexPixel].B;
      FirstLine[LPixel].G := FTexFirstLine[LTexPixel].G;
      FirstLine[LPixel].R := FTexFirstLine[LTexPixel].R;
    except
//        SiMain.LogCardinal('LU', LU);
//        SiMain.LogCardinal('LV', LV);
//        SiMain.LogSingle('LZ', LZ);
    end;
end;

initialization
si.Enabled := True;

end.
