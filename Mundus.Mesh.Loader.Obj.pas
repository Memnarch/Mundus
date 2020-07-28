unit Mundus.Mesh.Loader.Obj;

interface

uses
  SysUtils,
  Classes,
  Types,
  Mundus.Math,
  Mundus.Types,
  Mundus.Mesh,
  Mundus.Mesh.Loader;

type
  TFacePoint = record
    VIndex: Integer;
    UVIndex: Integer;
    NIndex: Integer;
  end;

  TObjMeshLoader = class(TAbstractMeshLoader)
  private
    class function ParseFace(const AText: string): TFacePoint;
    class procedure AddVertex(var AVertices: TArray<TVector>; const AParts: TStringDynArray; const AFormat: TFormatSettings);
    class procedure AddUV(var AUVs: TArray<TFloat2>; const AParts: TStringDynArray; const AFormat: TFormatSettings);
    class procedure AddFace(const AMesh: TMesh; const AVertices: TArray<TVector>; const AUVs: TArray<TFloat2>; const AParts: TStringDynArray);
  public
    class function CanLoad(const AFileName: string): Boolean; override;
    class function LoadFromFile(const AFileName: string): TMesh; override;
  end;

implementation

uses
  StrUtils;

{ TObjMeshLoader }

class procedure TObjMeshLoader.AddFace(const AMesh: TMesh;
  const AVertices: TArray<TVector>; const AUVs: TArray<TFloat2>;
  const AParts: TStringDynArray);
var
  LPoints: TArray<TFacePoint>;
  LIndices: TArray<Integer>;
  i: Integer;
begin
  SetLength(LPoints, Length(AParts) - 1);
  for i := 0 to High(LPoints) do
    LPoints[i] := ParseFace(AParts[i+1]);

  SetLength(LIndices, Length(LPoints));
  for i := 0 to High(LPoints) do
  begin
    LIndices[i] := AMesh.AddVertice(AVertices[LPoints[i].VIndex]);
    if LPoints[i].UVIndex > -1 then
      AMesh.AddUV(AUVs[LPoints[i].UVIndex]);
  end;

  if Length(LIndices) > 2 then
    AMesh.AddTriangle(Triangle(LIndices[0], LIndices[1], LIndices[2]));
  if Length(LIndices) > 3 then
    AMesh.AddTriangle(Triangle(LIndices[0], LIndices[2], LIndices[3]));
end;

class procedure TObjMeshLoader.AddUV(var AUVs: TArray<TFloat2>; const AParts: TStringDynArray; const AFormat: TFormatSettings);
begin
  AUVs := AUVs + [TFloat2.Create(StrToFloatDef(AParts[1], 0, AFormat), StrToFloatDef(AParts[2], 0, AFormat))]
end;

class procedure TObjMeshLoader.AddVertex(var AVertices: TArray<TVector>; const AParts: TStringDynArray; const AFormat: TFormatSettings);
begin
  AVertices := AVertices + [Vector(StrToFloatDef(AParts[1], 0, AFormat), StrToFloatDef(AParts[2], 0, AFormat), StrToFloatDef(AParts[3], 0, AFormat))];
end;

class function TObjMeshLoader.CanLoad(const AFileName: string): Boolean;
begin
  Result := AnsiSameText(ExtractFileExt(AFileName), '.obj');
end;

class function TObjMeshLoader.LoadFromFile(const AFileName: string): TMesh;
var
  LFile: TStringList;
  LLine: string;
  LParts: TStringDynArray;
  LFormat: TFormatSettings;
  LVertices: TArray<TVector>;
  LUVs: TArray<TFloat2>;
begin
  LFormat := TFormatSettings.Create();
  LFormat.DecimalSeparator := '.';
  Result := TMesh.Create();
  LFile := TStringList.Create();
  try
    LFile.LoadFromFile(AFileName);
    for LLine in LFile do
    begin
      LParts := SplitString(Trim(LLine), ' ');
      if Length(LParts) > 0 then
      begin
        case AnsiIndexText(LParts[0], ['v', 'vt', 'vn', 'f']) of
          //vertex
          0: AddVertex(LVertices, LParts, LFormat);
          //UV coordinate
          1: AddUV(LUVs, LParts, LFormat);
          //normal
//          2:
          //face
          3: AddFace(Result, LVertices, LUVs, LParts);
        end;
      end;
    end;
  finally
    LFile.Free;
  end;
end;

class function TObjMeshLoader.ParseFace(const AText: string): TFacePoint;
var
  LParts: TStringDynArray;
begin
  LParts := SplitString(AText, '/');
  Result.VIndex := StrToIntDef(LParts[0], 1) - 1;
  if Length(LParts) > 1 then
    Result.UVIndex := StrToIntDef(LParts[1], 1) - 1
  else
    Result.UVIndex := -1;

  if Length(LParts) > 2 then
    Result.NIndex := StrToIntDef(LParts[2], 1) - 1
  else
    Result.NIndex := -1;
end;

initialization
  TMeshLoaders.RegisterLoader(TObjMeshLoader);

end.
