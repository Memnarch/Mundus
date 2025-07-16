unit Mundus.Shader;

interface

uses
  Classes,
  Types,
  Graphics,
  Mundus.Types,
  Mundus.Math,
  Mundus.ValueBuffer;

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
    procedure SetConstants(AData: Pointer); virtual;
    procedure VertexShader(var AVertex: TFloat4; const AVInput: Pointer; const AVOutput: Pointer); virtual; abstract;
    class function GetBufferDescriptor: TValueBufferDescriptor; virtual; abstract;
    class function GetConstantBufferDescriptor: TValueBufferDescriptor; virtual; abstract;
    class function GetRasterizer: TRasterizer; virtual; abstract;
    class function GetFragmentAttributeSize: Integer; virtual; abstract;
    class function GetFragmentAttributeCount: Integer;
  end;

  TShader<TPSType, TVSType, TConstants: record> = class(TShader)
  public type
    TFragmentAttributes = TPSType;
    PFragmentAttributes = ^TPSType;
    TVertexAttributes = TVSType;
    PVertexAttributes = ^TVertexAttributes;
    TConstantAttributes = TConstants;
    PConstantAttributes = ^TConstantAttributes;
  private
    FConstants: TConstants;
  protected
    property Constants: TConstants read FConstants;
  public
    class function GetFragmentAttributeSize: Integer; override;
    class function GetBufferDescriptor: TValueBufferDescriptor; override;
    class function GetConstantBufferDescriptor: TValueBufferDescriptor; override;
    procedure SetConstants(AData: Pointer); override;
    procedure VertexShader(var AVertex: TFloat4; const AVInput: Pointer; const AVOutput: Pointer); override;
    procedure Vertex(var AVertex: TFloat4; const AVInput: PVertexAttributes; const AVOutput: PFragmentAttributes); virtual; abstract;
    procedure Fragment(const APixel: PRGB32; const PSInput: PFragmentAttributes); virtual; abstract;
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

constructor TShader.Create();
begin
  inherited;
end;

class function TShader.GetFragmentAttributeCount: Integer;
begin
  Result := GetFragmentAttributeSize div SizeOf(Single);
end;

procedure TShader.SetConstants(AData: Pointer);
begin

end;

class function TShader<TPSType, TVSType, TConstants>.GetBufferDescriptor: TValueBufferDescriptor;
begin
  Result := TValueBufferDescriptor.Create<TVSType>;
end;

class function TShader<TPSType, TVSType, TConstants>.GetConstantBufferDescriptor: TValueBufferDescriptor;
begin
  Result := TValueBufferDescriptor.Create<TConstants>;
end;

{ TSHader<T> }

class function TShader<TPSType, TVSType, TConstants>.GetFragmentAttributeSize: Integer;
begin
  Result := SizeOf(TPSType);
end;

procedure TShader<TPSType, TVSType, TConstants>.SetConstants(AData: Pointer);
begin
  inherited;
  FConstants := PConstantAttributes(AData)^;
end;

procedure TShader<TPSType, TVSType, TConstants>.VertexShader(var AVertex: TFloat4; const AVInput: Pointer; const AVOutput: Pointer);
begin
  Vertex(AVertex, PVertexAttributes(AVInput), PFragmentAttributes(AVOutput));
end;

end.
