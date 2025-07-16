unit Mundus.Shader.Normal;

interface

uses
  Mundus.Types,
  Mundus.Math,
  Mundus.Shader,
  Mundus.Shader.Color,
  Mundus.ValueBuffer;

type
  TVSInput = record
    Normal: TFloat3;
  end;

  TNormalShader = class(TColorShader<TVSInput>)
  public
    procedure Vertex(var AVertex: TFloat4; const AVInput: TNormalShader.PVertexAttributes; const AVOutput: TNormalShader.PFragmentAttributes); override; final;
    class function GetRasterizer: TRasterizer; override;
  end;

implementation

uses
  System.Math,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

{ TNormalShader }

procedure TNormalShader.Vertex(var AVertex: TFloat4; const AVInput: TNormalShader.PVertexAttributes; const AVOutput: TNormalShader.PFragmentAttributes);
begin
  inherited;
  AVOutput.Color.XYZ := AVInput.Normal;
end;

type
  TAttributes = TColorShaderPSInput;
  Shader = TNormalShader;

const
  DepthTest = dtWrite;

{$I Rasterizer.inc}

class function TNormalShader.GetRasterizer: TRasterizer;
begin
  Result := @RasterizeTriangle;
end;

end.
