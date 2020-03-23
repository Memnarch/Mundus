unit Mundus.Shader.DepthColor;

interface

uses
  Classes,
  Types,
  Mundus.Types,
  Mundus.Shader,
  Mundus.Math,
  Graphics,
  SysUtils;

type
  TDepthPSInput = TFloat4;
  PDepthPSInput = ^TDepthPSInput;

  TDepthColorShader = class sealed(TShader<TDepthPSInput>)
  public
    procedure Vertex(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TShader<TDepthPSInput>.PAttributeType); override; final;
    procedure Fragment(const APixel: PRGB32; const PSInput: TShader<TDepthPSInput>.PAttributeType); override; final;
    class function GetRasterizer: TRasterizer; override;
  end;

implementation

uses
  Mundus.Rasterizer;

{ TDepthColorShader }

{ TDepthColorShader }

procedure TDepthColorShader.Fragment(const APixel: PRGB32;
  const PSInput: TShader<TDepthPSInput>.PAttributeType);
asm
  //load input
  movups xmm2, [PSInput]
  //convert Single to DWord
  cvttps2dq xmm2, xmm2
  //Pack DWord to Word
  packusdw xmm2, xmm2
  //Pack Word to Byte
  packuswb xmm2, xmm2
  //write final color values
  PEXTRD [APixel], xmm2, 0
end;

class function TDepthColorShader.GetRasterizer: TRasterizer;
begin
  Result := TRasterizer(@TRasterizerFactory.RasterizeTriangle<TDepthPSInput, TDepthColorShader, TNoDepth>);
end;

procedure TDepthColorShader.Vertex(const AWorld, AProjection: TMatrix4x4;
  var AVertex: TFloat4; const AVInput: TVertexShaderInput;
  const AAttributeBuffer: TShader<TDepthPSInput>.PAttributeType);
begin
  inherited;
  AAttributeBuffer.R := 255-255*AVertex.Z/AVertex.W;
end;

end.
