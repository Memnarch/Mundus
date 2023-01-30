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
  TTexturePSInput = packed record
    UV: TUV;
    Padding: TFloat2;
  end;

  TTextureShader = class sealed(TShader<TTexturePSInput>)
  private
    FUVs: TArray<TFloat2>;
    FTexture: TTexture;
  public
    procedure BindBuffer(const ABuffer: PValueBuffers); override;
    procedure Vertex(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TShader<TTexturePSInput>.PAttributeType); override; final;
    procedure Fragment(const APixel: PRGB32; const PSInput: TShader<TTexturePSInput>.PAttributeType); override; final;
    class function GetRasterizer: TRasterizer; override; final;
  published
    property UV0: TArray<TFloat2> read FUVs write FUVs;
    property Tex0: TTexture read FTexture write FTexture;
  end;

implementation

uses
  Math,
  Mundus.Math.Interpolation,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

{ TTextureShader }
{$PointerMath ON}

procedure TTextureShader.BindBuffer(const ABuffer: PValueBuffers);
begin
  inherited;
  FUVs := ABuffer.Float2Array[ABuffer.Float2Array.GetBinding('UV0')];
  FTexture := ABuffer.Texture[ABuffer.Texture.GetBinding('Tex0')];
end;

procedure TTextureShader.Fragment(const APixel: PRGB32; const PSInput: TShader<TTexturePSInput>.PAttributeType);
begin
  FTexture.SampleDot(PSInput.UV, APixel);
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

procedure TTextureShader.Vertex(const AWorld, AProjection: TMatrix4x4;
  var AVertex: TFloat4; const AVInput: TVertexShaderInput;
  const AAttributeBuffer: TShader<TTexturePSInput>.PAttributeType);
begin
  AVertex := AProjection.Transform(AVertex);
  AAttributeBuffer.UV.U := FUVs[AVInput.VertexID].U * FTexture.MaxX;
  AAttributeBuffer.UV.V := FUVs[AVInput.VertexID].V * FTexture.MaxY;
end;

end.

