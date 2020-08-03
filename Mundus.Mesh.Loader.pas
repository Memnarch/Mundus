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
    class function LoadFromFile(const AFileName: string): TMeshGroup; virtual; abstract;
  end;

  TMeshLoaderClass = class of TAbstractMeshLoader;

  TMeshLoaders = class
  private
    class var FLoaders: TArray<TMeshLoaderClass>;
  public
    class function LoadFromFile(const AFileName: string; AUnitConversion: Single = CMeterToCM): TMeshGroup;
    class procedure RegisterLoader(const AClass: TMeshLoaderClass);
  end;

implementation

uses
  SysUtils,
  Mundus.Types;

{ TMeshLoader }

class function TMeshLoaders.LoadFromFile(const AFileName: string; AUnitConversion: Single = CMeterToCM): TMeshGroup;
var
  LLoader: TMeshLoaderClass;
  i: Integer;
  LVector: TVector;
  LMesh: TMesh;
begin
  for LLoader in FLoaders do
    if LLoader.CanLoad(AFileName) then
    begin
      Result := LLoader.LoadFromFile(AFileName);
      for LMesh in Result.Meshes do
      begin
        for i := 0 to Length(LMesh.Vertices) do
        begin
          LVector := LMesh.Vertices[i];
          LVector.Mul(AUnitConversion);
          LMesh.Vertices[i] := LVector;
        end;
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
