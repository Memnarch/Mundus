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
    FUP, FVP, FBP: TVectorClass4D;
    FSZ, FSU, FSV: TVectorClass3D;
    FTexture: TBitmap;
    FTexFirstLine: PRGB32Array;
    FTexLineLength: Cardinal;
  public
    constructor Create(APixelBuffer: TBitmap);
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

constructor TTextureShader.Create(APixelBuffer: TBitmap);
begin
  inherited;
  FUP := TVectorClass4D.Create();
  FVP := TVectorClass4D.Create();
  FBP := TVectorClass4D.Create();
  FSZ := TVectorClass3D.Create();
  FSU := TVectorClass3D.Create();
  FSV := TVectorClass3D.Create();
end;

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
  FUP.CopyFromVector4D(FVecB);
  FUP.SubtractVector4D(FVecA);
  FVP.CopyFromVector4D(FVecC);
  FVP.SubtractVector4D(FVecA);
  FBP.CopyFromVector4D(FVecA);

  FsZ.x := FUp.Element[1] * FVp.Element[2] - FVp.Element[1] * FUp.Element[2];
  FsZ.y := FVp.Element[0] * FUp.Element[2] - FUp.Element[0] * FVp.Element[2];
  FsZ.z := FUp.Element[0] * FVp.Element[1] - FVp.Element[0] * FUp.Element[1];
  FsU.x := FVp.Element[1] * FBp.Element[2] - FBp.Element[1] * FVp.Element[2];
  FsU.y := FBp.Element[0] * FVp.Element[2] - FVp.Element[0] * FBp.Element[2];
  FsU.z := FVp.Element[0] * FBp.Element[1] - FBp.Element[0] * FVp.Element[1];
  FsV.x := FBp.Element[1] * FUp.Element[2] - FUp.Element[1] * FBp.Element[2];
  FsV.y := FUp.Element[0] * FBp.Element[2] - FBp.Element[0] * FUp.Element[2];
  FsV.z := FBp.Element[0] * FUp.Element[1] - FUp.Element[0] * FBp.Element[1];
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
  LX, LY, LPixel, LTexPixel, LTexX, LTexY: Cardinal;
  LSU, LSV: Single;
  LSZ: Single;
begin
  for LY  := Pixel.Y to Pixel.Y + 7 do
  begin
    for LX := Pixel.X to Pixel.X + 7 do
    begin
      LPixel := LY*LineLength + LX;
//      LSZ := FSZ.Z + FSZ.Y*LY + FSZ.X*LX;
//      LSU := FSU.Z + FSU.Y*LY + FSU.X*LX;
//      LSV := FSV.Z + FSV.Y*LY + FSV.X*LX;
//
//      LSZ := 1/LSZ;
//      LSU := LSU*LSZ;
//      LSV := LSV*LSZ;
//      LSZ := (FZA/FZD*LX + FZB/FZD*LY + FZC/FZD);//FZD;
      LSZ := ((FZA*LX) + (FZB*LY) + FZD)/FZC;
//      LSZ := 1/LSZ;
      LSU := Trunc(-(FUA*LX + FUB * LY + FUD)/FUC * (FTexture.Width-1));
      LSV := Trunc(-(FVA*LX + FVB * LY + FVD)/FVC * (FTexture.Height-1));
//      LSU := ((FUA*LX) + (FUB*LY) + FUD)/FUC;
//      LSV := ((FVA*LX) + (FVB*LY) + FVD)/FVC;
//      LSU := LSU * LSZ;
//      LSV := LSV * LSZ;
      LTexX := Trunc((FTexture.Width-1)*LSU);
      LTexY := Trunc((FTexture.Height-1)*LSV);

      try
//        if (LTexY < 32) and (LTexX < 32) then
//        begin
        LTexPixel := (LTexY mod 32)*FTexLineLength + (LTexX mod 32);
        //LValue := Trunc(-(FStepA * LX + FStepB * LY + FStepD)/FStepC);
        FirstLine[LPixel].B := FTexFirstLine[LTexPixel].B;
        FirstLine[LPixel].G := FTexFirstLine[LTexPixel].G;
        FirstLine[LPixel].R := FTexFirstLine[LTexPixel].R;
//        end;
      except
//        SiMain.LogCardinal('LU', LU);
//        SiMain.LogCardinal('LV', LV);
//        SiMain.LogSingle('LZ', LZ);
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
