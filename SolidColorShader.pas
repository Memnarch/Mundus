unit SolidColorShader;

interface

uses
  Types, Classes, ColorTypes, Shader;

type
  TSolidColorShader = class(TShader)
  private
    FColor: TRGB32;

  public
    property Color: TRGB32 read FColor write FColor;
    procedure Shade8X8Quad(); override;
    procedure ShadeSinglePixel(); override;
    procedure SetColor(R, G, B: Byte);
  end;

implementation

{ TSolidColorSHader }

procedure TSolidColorShader.SetColor(R, G, B: Byte);
begin
  FColor.B := B;
  FColor.R := R;
  FColor.G := G;
end;

procedure TSolidColorSHader.Shade8X8Quad;
var
  i, k: Integer;
  LPixel: Cardinal;
begin
  for i := Pixel.Y to Pixel.Y + 7 do
  begin
    LPixel := i*LineLength + Pixel.X;
    for k := 0 to 7 do
    begin
      FirstLine[LPixel].B := Color.B;
      FirstLine[LPixel].G := Color.G;
      FirstLine[LPixel].R := Color.R;
      LPixel := LPixel + 1;
    end;
  end;

end;

procedure TSolidColorSHader.ShadeSinglePixel;
var
  LPixel: Cardinal;
begin
  LPixel := Pixel.Y * LineLength + Pixel.X;
  FirstLine[LPixel].B := Color.B;
  FirstLine[LPixel].G := Color.G;
  FirstLine[LPixel].R := Color.R;
end;

end.
