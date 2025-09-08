unit Mundus.Shader.Texture;

interface

const
  CTextureShader = 'TextureShader';

implementation

uses
  Math,
  Mundus.Shader,
  Mundus.Math,
  Mundus.Types,
  Mundus.Texture,
  Mundus.Math.Interpolation,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

type
  TTextureConstantInput = record
    Projection: TMatrix4x4;
    Diffuse: TTexture;
  end;

  PTextureConstantInput = ^TTextureConstantInput;

  TTextureVSInput = record
    UV: TUV;
  end;

  TTexturePSInput = packed record
    UV: TUV;
    Padding: TFloat2;
  end;

  PTexturePSInput = ^TTexturePSInput;

procedure VertexShader(var AVertex: TFloat4; const [Ref] Constants: TTextureConstantInput; const [ref] AVSInput: TTextureVSInput; var AVSOutput: TTexturePSInput);
begin
  AVertex := Constants.Projection.Transform(AVertex);
  AVSOutput.UV.U := AVSInput.UV.U * Constants.Diffuse.MaxX;
  AVSOutput.UV.V := AVSInput.UV.V * Constants.Diffuse.MaxY;
end;

procedure FragmentShader(const Constants: PTextureConstantInput; const APixel: PRGB32; const PSInput: PTexturePSInput);
begin
  Constants.Diffuse.SampleDot(PSInput.UV, APixel);
end;

type
  TAttributes = TTexturePSInput;

const
  DepthTest = dtWrite;

{$i Rasterizer.inc}

initialization
  TShaders.Register<TTextureConstantInput, TTextureVSInput, TTexturePSInput>(CTextureShader, VertexShader, RasterizeTriangle);

end.

