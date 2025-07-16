unit Mundus.Shader.Texture.LightMapped;

interface

uses
  Mundus.Types,
  Mundus.Math,
  Mundus.Texture,
  Mundus.ValueBuffer,
  Mundus.Shader;

type
  TTextureConstantInput = record
    Projection: TMatrix4x4;
    Diffuse: TTexture;
    LightMap: TTexture;
  end;

  TTextureVSInput = record
    UV: TUV;
    UV2: TUV;
  end;

  TTexturePSInput = record
    UV: TUV;
    UV2: TUV;
  end;

  TLightMappedTextureShader = class(TShader<TTexturePSInput, TTextureVSInput, TTextureConstantInput>)
  public
    procedure Vertex(var AVertex: TFloat4; const AVInput: TLightMappedTextureShader.PVertexAttributes; const AVOutput: TLightMappedTextureShader.PFragmentAttributes); override; final;
    procedure Fragment(const APixel: PRGB32; const PSInput: TLightMappedTextureShader.PFragmentAttributes); override; final;
    class function GetRasterizer: TRasterizer; override; final;
  end;

implementation

uses
  System.Math,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

{ TLightMappedTextureSHader }

procedure TLightMappedTextureShader.Fragment(const APixel: PRGB32; const PSInput: TLightMappedTextureShader.PFragmentAttributes);
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
  Shader = TLightMappedTextureShader;

const
  DepthTest = dtWrite;

{$i Rasterizer.inc}

class function TLightMappedTextureShader.GetRasterizer: TRasterizer;
begin
  Result := @RasterizeTriangle;
end;

procedure TLightMappedTextureShader.Vertex(var AVertex: TFloat4; const AVInput: TLightMappedTextureShader.PVertexAttributes; const AVOutput: TLightMappedTextureShader.PFragmentAttributes);
begin
  AVertex := Constants.Projection.Transform(AVertex);
  AVOutput.UV.U := AVInput.UV.U * Constants.Diffuse.MaxX;
  AVOutput.UV.V := AVInput.UV.V * Constants.Diffuse.MaxY;
  AVOutput.UV2.U := AVInput.UV2.U * Constants.LightMap.MaxX;
  AVOutput.UV2.V := AVInput.UV2.V * Constants.LightMap.MaxY;
end;

end.
