unit Mundus.Shader.Texture.LightMapped;

interface

const
  CLightMappedTextureShader = 'LightMappedTextureShader';

implementation

uses
  System.Math,
  Mundus.Types,
  Mundus.Math,
  Mundus.Texture,
  Mundus.Shader,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

type
  TTextureConstantInput = record
    Projection: TMatrix4x4;
    Diffuse: TTexture;
    LightMap: TTexture;
  end;

  PTextureConstantInput = ^TTextureConstantInput;

  TTextureVSInput = record
    UV: TUV;
    UV2: TUV;
  end;

  TTexturePSInput = record
    UV: TUV;
    UV2: TUV;
  end;

  PTexturePSInput = ^TTexturePSInput;

procedure VertexShader(var AVertex: TFloat4; const [Ref] Constants: TTextureConstantInput; const [ref] AVInput: TTextureVSInput; var AVOutput: TTexturePSInput);
begin
  AVertex := Constants.Projection.Transform(AVertex);
  AVOutput.UV.U := AVInput.UV.U * Constants.Diffuse.MaxX;
  AVOutput.UV.V := AVInput.UV.V * Constants.Diffuse.MaxY;
  AVOutput.UV2.U := AVInput.UV2.U * Constants.LightMap.MaxX;
  AVOutput.UV2.V := AVInput.UV2.V * Constants.LightMap.MaxY;
end;

procedure FragmentShader(const Constants: PTextureConstantInput; const APixel: PRGB32; const PSInput: PTexturePSInput);
var
  LDiffuse, LLight: TRGB32;
begin
  Constants.Diffuse.SampleDot(PSInput.UV, @LDiffuse);
  Constants.LightMap.SampleDot(PSInput.UV2, @LLight);
  APixel.R := (LDiffuse.R * LLight.R) shr 8;
  APixel.G := (LDiffuse.G * LLight.G) shr 8;
  APixel.B := (LDiffuse.B * LLight.B) shr 8;
end;

type
  TAttributes = TTexturePSInput;

const
  DepthTest = dtWrite;

{$i Rasterizer.inc}

initialization
  TSHaders.Register<TTextureConstantInput, TTextureVSInput, TTexturePSInput>(CLightMappedTextureShader, VertexShader, RasterizeTriangle);

end.
