unit Mundus.Shader.Color;

interface

uses
  Mundus.Shader,
  Mundus.Types,
  Mundus.Math,
  Mundus.ValueBuffer;

type
  TColorShaderPSInput = packed record
    Color: TFloat4;
  end;

  TColorShaderConstantInput = record
    Projection: TMatrix4x4;
  end;

  PColorShaderPSInput = ^TColorShaderPSInput;

  TColorShader<TVSInput, TConstants: record> = class(TShader<TColorShaderPSInput, TVSInput, TConstants>)
  public
    procedure Fragment(const APixel: PRGB32; const PSInput: TColorShader<TVSInput, TConstants>.PFragmentAttributes); override;
  end;

  TColorShader<TVSInput: record> = class(TColorShader<TVSInput, TColorShaderConstantInput>)
  public
    procedure Vertex(var AVertex: TFloat4; const AVInput: TColorShader<TVSInput>.PVertexAttributes; const AVOutput: TColorShader<TVSInput>.PFragmentAttributes); override;
  end;

procedure DoColorFragment(const APixel: PRGB32; const PSInput: PColorShaderPSInput);

implementation

{ TColorShader }

const
  CDenormalizer: TFloat4 = (B: 255; G: 255; R: 255; A: 255);

procedure DoColorFragment(const APixel: PRGB32; const PSInput: PColorShaderPSInput);
asm
  //load input
  movups xmm2, [PSInput]
  //load denormalizer
  movups xmm1, [CDenormalizer];
  //denormalize PSInput
  mulps xmm2, xmm1
  //convert Single to DWord
  cvttps2dq xmm2, xmm2
  //Pack DWord to Word
  packusdw xmm2, xmm2
  //Pack Word to Byte
  packuswb xmm2, xmm2
  //write final color values
  PEXTRD [APixel], xmm2, 0
end;

procedure TColorShader<TVSInput, TConstants>.Fragment(const APixel: PRGB32; const PSInput: TColorShader<TVSInput, TConstants>.PFragmentAttributes);
begin
  DoColorFragment(APixel, PColorShaderPSInput(PSInput));
end;

{ TColorShader<TVSInput> }

procedure TColorShader<TVSInput>.Vertex(var AVertex: TFloat4;
  const AVInput: TColorShader<TVSInput>.PVertexAttributes;
  const AVOutput: TColorShader<TVSInput>.PFragmentAttributes);
begin
  inherited;
  AVertex := Constants.Projection.Transform(AVertex);
end;

end.
