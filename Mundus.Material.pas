unit Mundus.Material;

interface

uses
  Mundus.Types;

type
  TMaterial = record
    Name: string;
    AmbientColor: TVector;
    DiffuseColor: TVector;
    DiffuseTexture: Integer;
    class function Create: TMaterial; static;
  end;


implementation

{ TMaterial }

class function TMaterial.Create: TMaterial;
begin
  Result := Default(TMaterial);
  Result.DiffuseTexture := -1;
end;

end.
