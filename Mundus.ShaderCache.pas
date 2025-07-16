unit Mundus.ShaderCache;

interface

uses
  Generics.Collections,
  Mundus.Shader,
  Mundus.ValueBuffer;

type
  TShaderCacheEntry = record
    Instance: TShader;
    VertexBufferDescriptor: TValueBufferDescriptor;
    ConstantBufferDescriptor: TValueBufferDescriptor;
  end;

  PShaderCacheEntry = ^TShaderCacheEntry;

  TShaderCache = class
  private
    FInstances: TObjectList<TShader>;
    FShaders: TDictionary<TShaderClass, TShaderCacheEntry>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetShader(AClass: TShaderClass): TShaderCacheEntry;
  end;

implementation

{ TShaderCache }

constructor TShaderCache.Create;
begin
  inherited;
  FInstances := TObjectList<TShader>.Create();
  FShaders := TDictionary<TShaderClass, TShaderCacheEntry>.Create();
end;

destructor TShaderCache.Destroy;
begin
  FShaders.Free;
  FInstances.Free;
  inherited;
end;

function TShaderCache.GetShader(AClass: TShaderClass): TShaderCacheEntry;
begin
  if not FShaders.TryGetValue(AClass, Result) then
  begin
    Result.Instance := AClass.Create();
    Result.VertexBufferDescriptor := AClass.GetBufferDescriptor();
    Result.ConstantBufferDescriptor := AClass.GetConstantBufferDescriptor();
    FInstances.Add(Result.Instance);
    FShaders.Add(AClass, Result);
  end;
end;

end.
