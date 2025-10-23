unit Mundus.Shader.Normal;

interface

const
  CNormalShader = 'NormalShader';

implementation

uses
  System.Math,
  Mundus.Types,
  Mundus.Math,
  Mundus.Shader,
  Mundus.Shader.Color,
  Mundus.ValueBuffer,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

type
  TConstantInput = record
    Projection: TMatrix4x4;
  end;

  TVSInput = record
    Normal: TFloat3;
  end;

procedure VertexShader(var AVertex: TFloat4; const [Ref] Constants: TConstantInput; const [ref] AVSInput: TVSInput; var AVSOutput: TColorShaderPSInput);
begin
  AVertex := Constants.Projection * AVertex;
  AVSOutput.Color.XYZ := AVSInput.Normal;
end;

type
  TAttributes = TColorShaderPSInput;

const
  DepthTest = dtWrite;

{$I Rasterizer.inc}

initialization
  TShaders.Register<TConstantInput, TVSInput, TColorShaderPSInput>(CNormalShader, VertexShader, RasterizeTriangle);

end.
