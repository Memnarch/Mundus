unit Mundus.Shader.Occlusion;

interface

uses
  Mundus.Types,
  Mundus.Math,
  Mundus.Shader,
  Mundus.Rasterizer;

type
  TOcclusionShader = class sealed(TShader<TNoAttributes>)
  public
    procedure Vertex(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TShader<TNoAttributes>.PAttributeType); override; final;
    procedure Fragment(const APixel: PRGB32; const PSInput: TShader<TNoAttributes>.PAttributeType); override; final;
    class function GetRasterizer: TRasterizer; override; final;
  end;

implementation

{ TNullShader }

procedure TOcclusionShader.Fragment(const APixel: PRGB32;
  const PSInput: TShader<TNoAttributes>.PAttributeType);
begin
end;

class function TOcclusionShader.GetRasterizer: TRasterizer;
begin
  Result := TRasterizer(@TRasterizerFactory<TNoAttributes, TOcclusionShader, TDepthWrite>.RasterizeOcclusionTriangle);
end;

procedure TOcclusionShader.Vertex(const AWorld, AProjection: TMatrix4x4;
  var AVertex: TFloat4; const AVInput: TVertexShaderInput;
  const AAttributeBuffer: TShader<TNoAttributes>.PAttributeType);
begin
  AVertex := AProjection.Transform(AVertex);
end;

end.
