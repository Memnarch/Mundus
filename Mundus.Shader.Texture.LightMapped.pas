unit Mundus.Shader.Texture.LightMapped;

interface

uses
  Mundus.Types,
  Mundus.Math,
  Mundus.Texture,
  Mundus.ValueBuffer,
  Mundus.Shader;

type
  TTexturePSInput = record
    UV: TUV;
    UV2: TUV;
  end;

  TLightMappedTextureShader = class(TShader<TTexturePSInput>)
  private
    FUV: TArray<TFloat2>;
    FUV2: TArray<TFloat2>;
    FDiffuseTexture: TTexture;
    FLightMap: TTexture;
  public
    procedure BindBuffer(const ABuffer: PValueBuffers); override;
    procedure Vertex(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TShader<TTexturePSInput>.PAttributeType); override; final;
    procedure Fragment(const APixel: PRGB32; const PSInput: TShader<TTexturePSInput>.PAttributeType); override; final;
    class function GetRasterizer: TRasterizer; override; final;
  end;

implementation

uses
  System.Math,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

{ TLightMappedTextureSHader }

procedure TLightMappedTextureShader.BindBuffer(const ABuffer: PValueBuffers);
begin
  inherited;
  FUV := ABuffer.Float2Array[ABuffer.Float2Array.GetBinding('UV0')];
  FUV2 := ABuffer.Float2Array[ABuffer.Float2Array.GetBinding('UV1')];
  FDiffuseTexture := ABuffer.Texture[ABuffer.Texture.GetBinding('Tex0')];
  FLightMap := ABuffer.Texture[ABuffer.Texture.GetBinding('Tex1')];
end;

procedure TLightMappedTextureShader.Fragment(const APixel: PRGB32; const PSInput: TShader<TTexturePSInput>.PAttributeType);
var
  LDiffuse, LLight: TRGB32;
begin
  FDiffuseTexture.SampleDot(PSInput.UV, @LDiffuse);
  FLightMap.SampleDot(PSInput.UV2, @LLight);
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

procedure TLightMappedTextureShader.Vertex(const AWorld,
  AProjection: TMatrix4x4; var AVertex: TFloat4;
  const AVInput: TVertexShaderInput;
  const AAttributeBuffer: TShader<TTexturePSInput>.PAttributeType);
begin
  AVertex := AProjection.Transform(AVertex);
  AAttributeBuffer.UV.U := FUV[AVInput.VertexID].U * FDiffuseTexture.MaxX;
  AAttributeBuffer.UV.V := FUV[AVInput.VertexID].V * FDiffuseTexture.MaxY;
  AAttributeBuffer.UV2.U := FUV2[AVInput.VertexID].U * FLightMap.MaxX;
  AAttributeBuffer.UV2.V := FUV2[AVInput.VertexID].V * FLightMap.MaxY;
end;

end.
