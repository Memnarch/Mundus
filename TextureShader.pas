unit TextureShader;

interface

uses
  Classes,
  Types,
  SysUtils,
  Shader,
  Math3D,
  RenderTypes,
  Graphics,
  ColorTypes,
  Texture,
  ValueBuffer;

type
  TTexturePSInput = record
    UV: TFloat2;
    Padding: TFloat2;
  end;

  TTextureShader = class sealed(TShader<TTexturePSInput>)
  private
    FUVs: ^TFloat2;
    FTexture: TTexture;
  public
    procedure BindBuffer(const ABuffer: PValueBuffers); override;
    procedure Vertex(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TShader<TTexturePSInput>.PAttributeType); override; final;
    procedure Fragment(const APixel: PRGB32; const PSInput: TShader<TTexturePSInput>.PAttributeType); override; final;
    class function GetRasterizer: TRasterizer; override; final;
  end;

implementation

uses
  Interpolation, Math, Rasterizer;

{ TTextureShader }
{$PointerMath ON}

procedure TTextureShader.BindBuffer(const ABuffer: PValueBuffers);
begin
  inherited;
  FUVs := @ABuffer.Float2Array[ABuffer.Float2Array.GetBinding('UV0')][0];
  FTexture := ABuffer.Texture[ABuffer.Texture.GetBinding('Tex0')];
end;

procedure TTextureShader.Fragment(const APixel: PRGB32; const PSInput: TShader<TTexturePSInput>.PAttributeType);
var
  LTexX, LTextY: Integer;
const
  sizeofsingle = sizeof(single);
begin
  //truncate from single to int without using trunc which is a function and sloooow
  asm
    mov eax, [PSInput]
    movss xmm0, [eax]
    //SizeOf(SIngle) results in $31 instead of $4 wtf delphi?
    movss xmm1, [eax + SizeOfSingle]
    cvttss2si eax, xmm0
    mov LTexX, eax
    cvttss2si eax, xmm1
    mov LTextY, eax
  end;
  FTexture.Sample(LTexX, LTextY, APixel);
end;

class function TTextureShader.GetRasterizer: TRasterizer;
begin
  Result := TRasterizer(@TRasterizerFactory.RasterizeTriangle<TTexturePSInput, TTextureShader, TDepthWrite>);
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

