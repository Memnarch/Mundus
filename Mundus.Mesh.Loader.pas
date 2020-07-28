unit Mundus.Mesh.Loader;

interface

uses
  Mundus.Mesh;

const
  CMeterToCM = 100;

type
  TAbstractMeshLoader = class
  public
    class function CanLoad(const AFileName: string): Boolean; virtual; abstract;
    class function LoadFromFile(const AFileName: string): TMesh; virtual; abstract;
  end;

  TMeshLoaderClass = class of TAbstractMeshLoader;

  TMeshLoaders = class
  private
    class var FLoaders: TArray<TMeshLoaderClass>;
  public
    class function LoadFromFile(const AFileName: string; AUnitConversion: Single = CMeterToCM): TMesh;
    class procedure RegisterLoader(const AClass: TMeshLoaderClass);
  end;

implementation

uses
  SysUtils,
  Mundus.Types;

{ TMeshLoader }

class function TMeshLoaders.LoadFromFile(const AFileName: string; AUnitConversion: Single = CMeterToCM): TMesh;
var
  LLoader: TMeshLoaderClass;
  i: Integer;
  LVector: TVector;
begin
  Result := nil;
  for LLoader in FLoaders do
    if LLoader.CanLoad(AFileName) then
    begin
      Result := LLoader.LoadFromFile(AFileName);
      for i := 0 to Length(Result.Vertices) do
      begin
        LVector := Result.Vertices[i];
        LVector.Mul(AUnitConversion);
        Result.Vertices[i] := LVector;
      end;
      Exit;
    end;
  raise ENotSupportedException.Create('Unsupported file format ' + ExtractFileExt(AFileName));
end;

{ TAbstractMeshLoader }

class procedure TMeshLoaders.RegisterLoader(const AClass: TMeshLoaderClass);
begin
  SetLength(FLoaders, Length(FLoaders)+1);
  FLoaders[High(FLoaders)] := AClass;
end;

end.
