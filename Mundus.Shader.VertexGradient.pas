unit Mundus.Shader.VertexGradient;

interface

const
  CVertexGradientShader = 'VertexGradientShader';

implementation

uses
  Math,
  Mundus.Types,
  Mundus.Shader,
  Mundus.Shader.Color,
  Mundus.Math,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;


type
  TGradientVSInput = record
    Color: TFloat4;
  end;

  TGradientConstants = record
    Projection: TMatrix4x4;
    World: TMatrix4x4;
  end;

type
  TAttributes = TColorShaderPSInput;

const
  DepthTest = dtNone;

{$i Rasterizer.inc}

{$PointerMath On}


procedure VertexShader(var AVertex: TFloat4; const [Ref] Constants: TGradientConstants; const [ref] AVSInput: TGradientVSInput; var AVSOutput: TColorShaderPSInput);
var
  LDist, LIntensity: Single;
  LVec: TFloat4;
begin
  LVec := Constants.World * AVertex;
  LDist := LVec.Length;
  LIntensity := Max(130-LDist, 0) / 50;
  AVertex := Constants.Projection * AVertex;
  AVSOutput.Color := AVSInput.Color * LIntensity;
end;

initialization
  TShaders.Register<TGradientConstants, TGradientVSInput, TColorShaderPSInput>(CVertexGradientShader, VertexShader, RasterizeTriangle);

end.
