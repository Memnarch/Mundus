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
  FVecA := AVecA;
  FVecB := AvecB;
  FVecC := AVecC;

  FZA := CalculateFactorA(AVecA, AVecB, AVecC, AVecA.Z, AvecB.Z, AvecC.Z);
  FZB := CalculateFactorB(AVecA, AVecB, AVecC, AVecA.Z, AvecB.Z, AvecC.Z);
  FZC := CalculateFactorC(AVecA, AVecB, AVecC);
  FZD := CalculateFactorD(AVecA, AVecB, AVecC, AVecA.Z, AvecB.Z, AvecC.Z);
end;

procedure TTextureShader.InitUV(AUVA, AUVB, AUVC: TUV);
begin
  FUA := CalculateFactorA(FVecA, FVecB, FVecC, AUVA.U, AUVB.U, AUVC.U);
  FUB := CalculateFactorB(FVecA, FVecB, FVecC, AUVA.U, AUVB.U, AUVC.U);
  FUC := CalculateFactorC(FVecA, FVecB, FVecC);
  FUD := CalculateFactorD(FVecA, FVecB, FVecC, AUVA.U, AUVB.U, AUVC.U);

  FVA := CalculateFactorA(FVecA, FVecB, FVecC, AUVA.V, AUVB.V, AUVC.V);
  FVB := CalculateFactorB(FVecA, FVecB, FVecC, AUVA.V, AUVB.V, AUVC.V);
  FVC := CalculateFactorC(FVecA, FVecB, FVecC);
  FVD := CalculateFactorD(FVecA, FVecB, FVecC, AUVA.V, AUVB.V, AUVC.V);
end;

procedure TTextureShader.Shade8X8Quad;
var
  LX, LY, LPixel, LTexPixel: Cardinal;
  LU, LV: Cardinal;
  LZ: Single;
begin
  for LY  := Pixel.Y to Pixel.Y + 7 do
  begin
    for LX := Pixel.X to Pixel.X + 7 do
    begin
      LPixel := LY*LineLength + LX;
      LZ := (FZA*LX + FZB*LY + FZD)/FZC;
      LU := Trunc(-(FUA*LX + FUB * LY + FUD)/FUC * (FTexture.Width-1));
      LV := Trunc(-(FVA*LX + FVB * LY + FVD)/FVC * (FTexture.Height-1));
//      SiMain.LogCardinal('LV1', LV);
//      SiMain.LogCardinal('LV2', LV);
//      SiMain.LogSingle('LZ', LZ);
      //LV := Max(0, Min(Lv, FTexture.Height-1));
      //LU := 1;
//      SiMain.LogCardinal('LU', LU);
//      SiMain.LogCardinal('LV', LV);
//      SiMain.LogSingle('LZ', LZ);
      try
        LTexPixel := LV*FTexLineLength + LU;
        //LValue := Trunc(-(FStepA * LX + FStepB * LY + FStepD)/FStepC);
        FirstLine[LPixel].B := FTexFirstLine[LTexPixel].B;
        FirstLine[LPixel].G := FTexFirstLine[LTexPixel].G;
        FirstLine[LPixel].R := FTexFirstLine[LTexPixel].R;
      except
        SiMain.LogCardinal('LU', LU);
        SiMain.LogCardinal('LV', LV);
        SiMain.LogSingle('LZ', LZ);
      end;
    end;
  end;
end;

procedure TTextureShader.ShadeSinglePixel;
begin


end;

initialization
si.Enabled := True;

end.
