unit Mundus.Shader.Color;

interface

uses
  Mundus.Shader,
  Mundus.Types,
  Mundus.Math;

type
  TColorShaderPSInput = packed record
    Color: TFloat4;
  end;

  TColorShaderConstantInput = record
    Projection: TMatrix4x4;
  end;

  PColorShaderPSInput = ^TColorShaderPSInput;

procedure VertexShader(const [Ref] Constants: TColorShaderConstantInput; var AVertex: TFloat4; const [ref] AVSInput: TNoAttributes; var AVSOutput: TColorShaderPSInput);
procedure FragmentShader(const Constants: Pointer; const APixel: PRGB32; const PSInput: PColorShaderPSInput);

implementation

{ TColorShader }

procedure VertexShader(const [Ref] Constants: TColorShaderConstantInput; var AVertex: TFloat4; const [ref] AVSInput: TNoAttributes; var AVSOutput: TColorShaderPSInput);
begin
  AVertex := Constants.Projection.Transform(AVertex);
end;

const
  CDenormalizer: TFloat4 = (B: 255; G: 255; R: 255; A: 255);
  CSingleDenormalizer: Single = 255;

procedure FragmentShader(const Constants: Pointer; const APixel: PRGB32; const PSInput: PColorShaderPSInput);
begin
  PFragment(APixel)^ := PSInput.Color;
end;
//asm
//  //load input
//  movups xmm2, [PSInput]
//  //load denormalizer
//  movss xmm1, [CSingleDenormalizer]
//  shufps xmm1, xmm1, 0
////  movaps xmm1, [CDenormalizer];
//  //denormalize PSInput
//  mulps xmm2, xmm1
//  //convert Single to DWord
//  cvttps2dq xmm2, xmm2
//  //Pack DWord to Word
//  packusdw xmm2, xmm2
//  //Pack Word to Byte
//  packuswb xmm2, xmm2
//  //write final color values
//  PEXTRD [APixel], xmm2, 0
//end;

end.
