unit Mundus.Shader.Texture;

interface

uses
  Classes,
  Types,
  SysUtils,
  Graphics,
  Mundus.Shader,
  Mundus.Math,
  Mundus.Types,
  Mundus.Texture,
  Mundus.ValueBuffer;

type
  TTextureConstantInput = record
    Projection: TMatrix4x4;
    Diffuse: TTexture;
  end;

  TTextureVSInput = record
    UV: TUV;
  end;

  TTexturePSInput = packed record
    UV: TUV;
    Padding: TFloat2;
  end;

  TTextureShader = class sealed(TShader<TTexturePSInput, TTextureVSInput, TTextureConstantInput>)
  public
    procedure Vertex(var AVertex: TFloat4; const AVInput: TTextureShader.PVertexAttributes; const AVOutput: TTextureShader.PFragmentAttributes); override; final;
    procedure Fragment(const APixel: PRGB32; const PSInput: TTextureShader.PFragmentAttributes); override; final;
    class function GetRasterizer: TRasterizer; override; final;
  end;

implementation

uses
  Math,
  Mundus.Math.Interpolation,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

{ TTextureShader }
{$PointerMath ON}

procedure TTextureShader.Fragment(const APixel: PRGB32; const PSInput: TTextureShader.PFragmentAttributes);
begin
  Constants.Diffuse.SampleDot(PSInput.UV, APixel);
end;

type
  TAttributes = TTexturePSInput;
  Shader = TTextureShader;

const
  DepthTest = dtWrite;

{$i Rasterizer.inc}

class function TTextureShader.GetRasterizer: TRasterizer;
begin
  Result := @RasterizeTriangle;
end;

procedure TTextureShader.Vertex(var AVertex: TFloat4; const AVInput: TTextureShader.PVertexAttributes; const AVOutput: TTextureShader.PFragmentAttributes);
begin
  AVertex := Constants.Projection.Transform(AVertex);
  AVOutput.UV.U := AVInput.UV.U * Constants.Diffuse.MaxX;
  AVOutput.UV.V := AVInput.UV.V * Constants.Diffuse.MaxY;
end;

end.

