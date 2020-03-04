unit DepthColorShader;

interface

uses
  Classes, Types, Shader, Math3D, Graphics, SysUtils;

type
  TDepthColorShader = class(TShader)
  private
    FStepA: Single;
    FStepB: Single;
    FStepC: Single;
    FStepD: Single;
    FVecA: TFloat4;
    FVecB: TFloat4;
    FVecC: TFloat4;
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    procedure Shade8X8Quad(); override;
    procedure ShadeSinglePixel(); override;
    procedure InitTriangle(AVecA, AVecB, AVecC: TFloat4); override;
  end;

implementation

uses
  Interpolation;
{ TDepthColorShader }

constructor TDepthColorShader.Create();
begin
  inherited;
end;

destructor TDepthColorShader.Destroy;
begin
  inherited;
end;

procedure TDepthColorShader.InitTriangle(AVecA, AVecB, AVecC: TFloat4);
var
  R1, R2, R3: Single;
begin
  inherited;
  FVecA := AVecA;
  FVecB := AVecB;
  FVecC := AvecC;
  R1 := 255-255*AvecA.Z;
  R2 := 255-255*AVecB.Z;
  R3 := 255-255*AVecC.Z;
  PixelBuffer.Canvas.Font.Color := clRed;
  PixelBuffer.Canvas.TextOut(0, 0, 'Z1: ' + FloatToStr(AvecA.Z));

  FStepA := CalculateFactorA(AVecA, AVecB, AVecC, R1, R2, R3);
  FStepB := CalculateFactorB(AVecA, AVecB, AVecC, R1, R2, R3);
  FStepC := CalculateFactorC(AVecA, AVecB, AVecC);
  FStepD := CalculateFactorD(AVecA, AVecB, AVecC, R1, R2, R3);
end;

procedure TDepthColorShader.Shade8X8Quad;
var
  LX, LY, LPixel, LValue: Integer;
begin
  for LY  := Pixel.Y to Pixel.Y + 7 do
  begin
    for LX := Pixel.X to Pixel.X + 7 do
    begin
      LPixel := LY*LineLength + LX;
      LValue := Trunc(-(FStepA * LX + FStepB * LY + FStepD)/FStepC);
      FirstLine[LPixel].B := LValue;
      FirstLine[LPixel].G := LValue;
      FirstLine[LPixel].R := LValue;
    end;
  end;
end;

procedure TDepthColorShader.ShadeSinglePixel;
var
  LPixel: Cardinal;
  LValue: Cardinal;
begin
  LPixel := Pixel.Y*LineLength + Pixel.X;
  LValue := Trunc(-(FStepA * Pixel.X + FStepB * Pixel.Y + FStepD)/FStepC);
  FirstLine[LPixel].B := LValue;
  FirstLine[LPixel].G := LValue;
  FirstLine[LPixel].R := LValue;
end;

end.
