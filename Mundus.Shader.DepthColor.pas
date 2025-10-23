unit Mundus.Shader.DepthColor;

interface

const
  CDepthColorShader = 'DepthColorShader';

implementation

uses
  System.Math,
  Mundus.Types,
  Mundus.Shader,
  Mundus.Shader.Color,
  Mundus.Math,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

type
  TDepthConstants = record
    Projection: TMatrix4x4;
    ZDistance: Single;
  end;

procedure VertexShader(var AVertex: TFloat4; const [Ref] Constants: TDepthConstants; const [ref] AVSInput: TNoAttributes; var AVOutput: TColorShaderPSInput);
begin
  AVertex := Constants.Projection * AVertex;
  AVOutput.Color.R := 255*(1-AVertex.Z/Constants.ZDistance);
end;


type
  TAttributes = TColorShaderPSInput;

const
  DepthTest = dtWrite;

{$I Rasterizer.inc}

initialization
  TSHaders.Register<TDepthConstants, TNoAttributes, TColorShaderPSInput>(CDepthColorShader, VertexShader, RasterizeTriangle);

end.
