unit Mundus.Shader.VertexGradient;

interface

uses
  Types,
  Classes,
  Mundus.Types,
  Mundus.Shader,
  Mundus.Math,
  Mundus.ValueBuffer;

type
  TGradientPSInput = TFloat4;
  PGradientPSInput = ^TGradientPSInput;

  TVertexGradientShader = class sealed(TShader<TGradientPSInput>)
  private
    FColors: ^TFloat4;
  public
    constructor Create; override;
    procedure BindBuffer(const ABuffer: PValueBuffers); override;
    procedure Vertex(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TShader<TGradientPSInput>.PAttributeType); override; final;
    procedure Fragment(const APixel: PRGB32; const PSInput: TShader<TGradientPSInput>.PAttributeType); override; final;
    class function GetRasterizer: TRasterizer; override;
  end;

implementation

uses
  Math,
  Mundus.Rasterizer;

  {$PointerMath On}

const
  CDenormalizer: TFloat4 = (B: 255; G: 255; R: 255; A: 255);


{ TSolidColorSHader }

procedure TVertexGradientShader.BindBuffer(const ABuffer: PValueBuffers);
begin
  inherited;
  FColors := @ABuffer.Float4Array[ABuffer.Float4Array.GetBinding('Color0')][0];
end;

constructor TVertexGradientShader.Create;
begin
  inherited;

end;

procedure TVertexGradientShader.Fragment(const APixel: PRGB32; const PSInput: TShader<TGradientPSInput>.PAttributeType);
asm
  //load input
  movups xmm2, [PSInput]
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
  PEXTRD [APixel], xmm2, 0
end;

class function TVertexGradientShader.GetRasterizer: TRasterizer;
begin
  Result := TRasterizer(@TRasterizerFactory<TGradientPSInput, TVertexGradientShader, TNoDepth>.RasterizeTriangle);
end;

procedure TVertexGradientShader.Vertex(const AWorld, AProjection: TMatrix4x4;
  var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TVertexGradientShader.PAttributeType);
var
  LDist, LIntensity: Single;
  LVec, LColors: TFloat4;
begin
  LVec := AWorld.Transform(AVertex);
  LDist := LVec.Length;
  LIntensity := Max(130-LDist, 0) / 50;
  inherited;
  LColors := FColors[AVInput.VertexID];
  LColors.Mul(LIntensity);
  AAttributeBuffer^ := LColors;
end;

end.
