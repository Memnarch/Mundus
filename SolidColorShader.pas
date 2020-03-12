unit SolidColorShader;

interface

uses
  Types,
  Classes,
  ColorTypes,
  Shader,
  Math3D,
  RenderTypes,
  ValueBuffer;

type
  TSolidColorPSInput = TFloat4;
  PSolidColorPSInput = ^TSolidColorPSInput;

  TSolidColorShader = class sealed(TShader<TSolidColorPSInput>)
  private
    FColors: ^TFloat4;
  public
    constructor Create; override;
    procedure BindBuffer(const ABuffer: PValueBuffers); override;
    procedure Vertex(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TShader<TSolidColorPSInput>.PAttributeType); override; final;
    procedure Fragment(X, Y: Integer; const PSInput: TShader<TSolidColorPSInput>.PAttributeType); override; final;
    class function GetRasterizer: TRasterizer; override;
  end;

implementation

uses
  Rasterizer,
  Math;

  {$PointerMath On}

const
  CDenormalizer: TFloat4 = (B: 255; G: 255; R: 255; A: 255);


{ TSolidColorSHader }

procedure TSolidColorShader.BindBuffer(const ABuffer: PValueBuffers);
begin
  inherited;
  FColors := @ABuffer.Float4Array[ABuffer.Float4Array.GetBinding('Color0')][0];
end;

constructor TSolidColorShader.Create;
begin
  inherited;

end;

procedure TSolidColorShader.Fragment(X, Y: Integer; const PSInput: TShader<TSolidColorPSInput>.PAttributeType);
var
  LPixel: NativeInt;
  LTarget: PRGB32;
begin
  LPixel := Y * LineLength + X;
  LTarget := @FirstLine[LPixel];
  asm
    //load input
    mov eax, [PSInput]
    movups xmm2, [eax]
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
    mov eax, [LTarget]
    PEXTRD [eax], xmm2, 0
  end;
end;

class function TSolidColorShader.GetRasterizer: TRasterizer;
begin
  Result := TRasterizer(@TRasterizerFactory.RasterizeTriangle<TSolidColorPSInput, TSolidColorShader>);
end;

procedure TSolidColorShader.Vertex(const AWorld, AProjection: TMatrix4x4;
  var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: TSolidColorShader.PAttributeType);
var
  LDist, LIntensity: Single;
  LVec, LColors: TFloat4;
begin
  LVec := AWorld.Transform(AVertex);
  LDist := LVec.Length;
  LIntensity := Max(130-LDist, 0) / 50;
  inherited;
  LColors := FColors[AVInput.VertexID];
  LColors.Mul(LIntensity);
  AAttributeBuffer^ := LColors;
end;

end.
