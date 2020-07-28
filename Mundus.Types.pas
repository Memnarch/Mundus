unit Mundus.Types;

interface

uses
  Mundus.Math;

type
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;

  PRGB32 = ^TRGB32;

  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGB32)-1] of TRGB32;
  PRGB32Array = ^TRGB32Array;

  TDepthBuffer = array of Single;
  PDepthsBuffer = ^TDepthBuffer;

  TVertexAttributeBuffer = TArray<Byte>;
  TRasterizer = procedure(
    AMaxResolutionX, AMaxResolutionY: Integer;
    const AVerctorA, AvectorB, AvectorC: TFloat4;
    const AAttributesA, AAttributesB, AAttributesC: TVertexAttributeBuffer;
    AShader: TObject;
    APixelBuffer: PRGB32Array;
    ADepthBuffer: PSingle;
    ABlockOffset, ABlockStep: Integer);

  TVector = TFloat3;

  PVector = ^TVector;

  TUV = record
    U, V: Single;
  end;

  TTriangle = record
    VertexA, VertexB, VertexC: Integer;
  end;

  PTriangle = ^TTriangle;

  function Vector(AX, AY, AZ: Single): TVector;
  function UV(AU, AV: Single): TUV;
  function Triangle(AVertexA, AVertexB, AVertexC: Integer): TTriangle;

  function RGB32(R, G, B, A: Byte): TRGB32;

const
  CQuadSize = 8;

implementation

{ Some Functions }
function Vector(AX, AY, AZ: Single): TVector;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function Triangle(AVertexA, AVertexB, AVertexC: Integer): TTriangle;
begin
  Result.VertexA := AVertexA;
  Result.VertexB := AVertexB;
  Result.VertexC := AVertexC;
end;

function UV(AU, AV: Single): TUV;
begin
  Result.U := AU;
  Result.V := AV;
end;

function RGB32(R, G, B, A: Byte): TRGB32;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

end.
