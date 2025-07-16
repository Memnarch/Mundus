unit Mundus.Shader.VertexGradient;

interface

uses
  Types,
  Classes,
  Mundus.Types,
  Mundus.Shader,
  Mundus.Shader.Color,
  Mundus.Math,
  Mundus.ValueBuffer;

type
  TGradientVSInput = record
    Color: TFloat4;
  end;

  TGradientConstants = record
    Projection: TMatrix4x4;
    World: TMatrix4x4;
  end;

  TVertexGradientShader = class sealed(TColorShader<TGradientVSInput, TGradientConstants>)
  public
    procedure Vertex(var AVertex: TFloat4; const AVInput: TVertexGradientShader.PVertexAttributes; const AVOutput: TVertexGradientShader.PFragmentAttributes); override; final;
    class function GetRasterizer: TRasterizer; override;
  end;

implementation

uses
  Math,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

{ TSolidColorSHader }


type
  TAttributes = TColorShaderPSInput;
  Shader = TVertexGradientShader;

const
  DepthTest = dtNone;

{$i Rasterizer.inc}

{$PointerMath On}

class function TVertexGradientShader.GetRasterizer: TRasterizer;
begin
  Result := @RasterizeTriangle;
end;

procedure TVertexGradientShader.Vertex(var AVertex: TFloat4; const AVInput: TVertexGradientShader.PVertexAttributes; const AVOutput: TVertexGradientShader.PFragmentAttributes);
var
  LDist, LIntensity: Single;
  LVec, LColor: TFloat4;
begin
  LVec := Constants.World.Transform(AVertex);
  LDist := LVec.Length;
  LIntensity := Max(130-LDist, 0) / 50;
  inherited;
  LColor := AVInput.Color;
  LColor.Mul(LIntensity);
  AVOutput.Color := LColor;
end;

end.
