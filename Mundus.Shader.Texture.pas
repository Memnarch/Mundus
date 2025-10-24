unit Mundus.Shader.Texture;

interface

uses
  Mundus.Texture,
  Mundus.Math,
  Mundus.Types;

const
  CTextureShader = 'TextureShader';

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

implementation

uses
  Math,
  Mundus.Shader,
  Mundus.Math.Interpolation,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

procedure VertexShader(var AVertex: TFloat4; const [Ref] Constants: TTextureConstantInput; const [ref] AVSInput: TTextureVSInput; var AVSOutput: TTexturePSInput);
begin
  AVertex := Constants.Projection * AVertex;
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

