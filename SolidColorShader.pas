unit SolidColorShader;

interface

uses
  Types, Classes, ColorTypes, Shader, Math3D, RenderTypes;

type
  TSolidColorPSInput = TFloat4;
  PSolidColorPSInput = ^TSolidColorPSInput;

  TSolidColorShader = class sealed(TShader<TSolidColorPSInput>)
  private
    FColor: TRGB32;
  public
    constructor Create; override;
    property Color: TRGB32 read FColor write FColor;
    procedure Shade8X8Quad(); override;
    procedure ShadeSinglePixel(); override;
    procedure SetColor(R, G, B: Byte);
    procedure Vertex(const AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TShader<TSolidColorPSInput>.PAttributeType); override; final;
    procedure Fragment(X, Y: Integer; const PSInput: TShader<TSolidColorPSInput>.PAttributeType); override; final;
    class function GetRasterizer: TRasterizer; override;
  end;

implementation

uses
  Rasterizer;

const
  CDenormalizer: TFloat4 = (B: 255; G: 255; R: 255; A: 255);
  CColors: array[0..2] of TFloat4 = (
    (B: 1; G: 0.5; R: 0.5; A: 0),
    (B: 0.5; G: 1; R: 0.5; A: 0),
    (B: 1; G: 1; R: 0.5; A: 0)
    );

var
  GNextColor : Integer = 0;


{ TSolidColorSHader }

constructor TSolidColorShader.Create;
begin
  inherited;
  FColor.B := 255;
  FColor.G := 128;
  FColor.R := 128;
end;

procedure TSolidColorShader.Fragment(X, Y: Integer; const PSInput: TShader<TSolidColorPSInput>.PAttributeType);
var
  LPixel: NativeInt;
  LTarget: PRGB32;
begin
  LPixel := Y * LineLength + X;
  LTarget := @FirstLine[LPixel];
  asm
    //load input
    mov eax, [PSInput]
    movups xmm2, [eax]
    //load denormalizer
    movups xmm1, [CDenormalizer];
    //denormalize PSInput
    mulps xmm2, xmm1
    //convert Single to DWord
    cvttps2dq xmm2, xmm2
    //Pack DWord to Word
    packusdw xmm2, xmm2
    //Pack Word to Byte
    packuswb xmm2, xmm2
    //write final color values
    mov eax, [LTarget]
    PEXTRD [eax], xmm2, 0
  end;
end;

class function TSolidColorShader.GetRasterizer: TRasterizer;
begin
  Result := TRasterizer(@TRasterizerFactory.RasterizeTriangle<TSolidColorPSInput, TSolidColorShader>);
end;

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

procedure TSolidColorShader.Vertex(const AProjection: TMatrix4x4;
  var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TSolidColorShader.PAttributeType);
begin
  inherited;
  AAttributeBuffer^ := CColors[AVInput.VertexID mod Length(CColors)];
end;

end.
