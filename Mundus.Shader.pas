unit Mundus.Shader;

interface

uses
  Classes,
  Types,
  Graphics,
  Mundus.Types,
  Mundus.Math,
  Mundus.ValueBuffer,
  Mundus.PixelBuffer;

type

  TPointF = record
    X: Single;
    Y: Single;
  end;

  TVertexShaderInput = record
    VertexID: Integer;
  end;

  TNoAttributes = record
  end;

  TShader = class(TObject)
  public
    constructor Create(); virtual;
    procedure VertexShader(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: Pointer); virtual; abstract;
    procedure BindBuffer(const ABuffer: PValueBuffers); virtual;
    class function GetRasterizer: TRasterizer; virtual; abstract;
    class function GetAttributeBufferSize: Integer; virtual; abstract;
    class function GetAttributeCount: Integer;
  end;

  TShader<T: record> = class(TShader)
  public type
    PAttributeType = ^T;
  public
    class function GetAttributeBufferSize: Integer; override;
    procedure VertexShader(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: Pointer); override;
    procedure Vertex(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: PAttributeType); virtual;
    procedure Fragment(const APixel: PRGB32; const PSInput: PAttributeType); virtual; abstract;
  end;

  TShaderClass = class of TShader;

  function PointF(X, Y: Single): TPointF;

implementation

uses
  System.TypInfo,
  System.Rtti;

function PointF(X, Y: Single): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

{ TShader }

procedure TShader.BindBuffer(const ABuffer: PValueBuffers);
begin

end;

constructor TShader.Create();
begin
  inherited;
end;

class function TShader.GetAttributeCount: Integer;
begin
  Result := GetAttributeBufferSize div SizeOf(Single);
end;

{ TSHader<T> }

class function TShader<T>.GetAttributeBufferSize: Integer;
begin
  Result := SizeOf(T);
end;

procedure TShader<T>.Vertex(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: PAttributeType);
begin
  AVertex := AProjection.Transform(AVertex);
end;

procedure TShader<T>.VertexShader(const AWorld, AProjection: TMatrix4x4; var AVertex: TFloat4; const AVInput: TVertexShaderInput; const AAttributeBuffer: Pointer);
begin
  Vertex(AWorld, AProjection, AVertex, AVInput, AAttributeBuffer);
end;

end.
