unit Mundus.Shader.DepthColor;

interface

uses
  Classes,
  Types,
  Mundus.Types,
  Mundus.Shader,
  Mundus.ValueBuffer,
  Mundus.Math,
  Graphics,
  SysUtils;

type
  TDepthPSInput = TFloat4;
  PDepthPSInput = ^TDepthPSInput;

  TDepthConstants = record
    Projection: TMatrix4x4;
    ZDistance: Single;
  end;

  TDepthColorShader = class sealed(TShader<TDepthPSInput, TNoAttributes, TDepthConstants>)
  public
    procedure Vertex(var AVertex: TFloat4; const AVInput: TDepthColorShader.PVertexAttributes; const AVOutput: TDepthColorShader.PFragmentAttributes); override; final;
    procedure Fragment(const APixel: PRGB32; const PSInput: TDepthColorShader.PFragmentAttributes); override; final;
    class function GetRasterizer: TRasterizer; override;
  end;

implementation

uses
  System.Math,
  Mundus.Rasterizer.Types,
  Mundus.Rasterizer.Helper;

{ TDepthColorShader }

{ TDepthColorShader }

procedure TDepthColorShader.Fragment(const APixel: PRGB32; const PSInput: TDepthColorShader.PFragmentAttributes);
asm
  //load input
  movups xmm2, [PSInput]
  //convert Single to DWord
  cvttps2dq xmm2, xmm2
  //Pack DWord to Word
  packusdw xmm2, xmm2
  //Pack Word to Byte
  packuswb xmm2, xmm2
  //write final color values
  PEXTRD [APixel], xmm2, 0
end;

type
  TAttributes = TDepthPSInput;
  Shader = TDepthColorShader;

const
  DepthTest = dtWrite;

{$I Rasterizer.inc}

class function TDepthColorShader.GetRasterizer: TRasterizer;
begin
  Result := @RasterizeTriangle;
end;

procedure TDepthColorShader.Vertex(var AVertex: TFloat4; const AVInput: TDepthColorShader.PVertexAttributes; const AVOutput: TDepthColorShader.PFragmentAttributes);
begin
  AVertex := Constants.Projection.Transform(AVertex);
  AVOutput.R := 255*(1-AVertex.Z/Constants.ZDistance);
end;

end.
