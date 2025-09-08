unit Mundus.Shader;

interface

uses
  Classes,
  Types,
  Graphics,
  Mundus.Types,
  Mundus.Math,
  Mundus.ValueBuffer,
  System.Generics.Collections;

type
  TNoAttributes = record
  end;

  TVertexShader<TConstants, TVSInput, TVSOutput> = procedure(var AVertex: TFloat4; const [ref] ConstantValues: TConstants; const [ref] VSInput: TVSInput; var VSOutput: TVSOutput);
  TGenericVertexShader = procedure(var AVertex: TFloat4; const ConstantValues, VSInput, VSOutput: Pointer);

  TShaderInfo = record
    VertexShader: TGenericVertexShader;
    Rasterizer: TRasterizer;
    ConstantBufferDescriptor: TValueBufferDescriptor;
    VertexBufferDescriptor: TValueBufferDescriptor;
    FragmentAttributeSize: Integer;
  end;

  PShaderInfo = ^TShaderInfo;

  TShaders = record
  private
    class var FShaders: TDictionary<string, PShaderInfo>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure Register<TConstants, TVSInput, TVSOutput: record>(const AName: string; const AVertexShader: TVertexShader<TConstants, TVSInput, TVSOutput>; const ARasterizer: TRasterizer); static;
    class function Resolve(const AName: string): PShaderInfo; static;
  end;

implementation

uses
  System.TypInfo,
  System.Rtti;

{ TShaders }

class constructor TShaders.Create;
begin
  FShaders := TDictionary<string, PShaderInfo>.Create();
end;

class destructor TShaders.Destroy;
var
  LShader: PShaderInfo;
begin
  for LShader in FShaders.Values do
    FreeMemory(LShader);
  FShaders.Free;
end;

class procedure TShaders.Register<TConstants, TVSInput, TVSOutput>(
  const AName: string;
  const AVertexShader: TVertexShader<TConstants, TVSInput, TVSOutput>;
  const ARasterizer: TRasterizer);
var
  LShader: PShaderInfo;
begin
  LShader := GetMemory(SizeOf(TShaderInfo));
  LShader.VertexShader := TGenericVertexShader(AVertexShader);
  LShader.Rasterizer := ARasterizer;
  LShader.ConstantBufferDescriptor := TValueBufferDescriptor.Create<TConstants>();
  LShader.VertexBufferDescriptor := TValueBufferDescriptor.Create<TVSInput>();
  LShader.FragmentAttributeSize := SizeOf(TVSOutput);
  FShaders.Add(AName, LShader);
end;

class function TShaders.Resolve(const AName: string): PShaderInfo;
begin
  Result := FShaders[AName];
end;

end.
