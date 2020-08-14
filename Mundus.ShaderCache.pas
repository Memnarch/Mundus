unit Mundus.ShaderCache;

interface

uses
  Generics.Collections,
  Mundus.Shader;

type
  TShaderCache = class
  private
    FInstances: TObjectList<TShader>;
    FShaders: TDictionary<TShaderClass, TShader>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetShader(AClass: TShaderClass): TShader;
  end;

implementation

{ TShaderCache }

constructor TShaderCache.Create;
begin
  inherited;
  FInstances := TObjectList<TShader>.Create();
  FShaders := TDictionary<TShaderClass, TShader>.Create();
end;

destructor TShaderCache.Destroy;
begin
  FShaders.Free;
  FInstances.Free;
  inherited;
end;

function TShaderCache.GetShader(AClass: TShaderClass): TShader;
begin
  if not FShaders.TryGetValue(AClass, Result) then
  begin
    Result := AClass.Create();
    FInstances.Add(Result);
    FShaders.Add(AClass, Result);
  end;
end;

end.
