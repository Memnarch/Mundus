unit Mundus.Mesh.Loader.Obj;

interface

uses
  SysUtils,
  Classes,
  Types,
  Mundus.Math,
  Mundus.Types,
  Mundus.Mesh,
  Mundus.Mesh.Loader,
  Mundus.Material;

type
  TFacePoint = record
    VIndex: Integer;
    UVIndex: Integer;
    NIndex: Integer;
  end;

  TObjMeshLoader = class(TAbstractMeshLoader)
  private
    class function LoadMaterial(const AFileName: string): TArray<TMaterial>;
    class function ParseFace(const AText: string): TFacePoint;
    class procedure AddVertex(var AVertices: TArray<TVector>; const AParts: TStringDynArray; const AFormat: TFormatSettings);
    class procedure AddUV(var AUVs: TArray<TFloat2>; const AParts: TStringDynArray; const AFormat: TFormatSettings);
    class procedure AddFace(const AMesh: TMesh; const AVertices: TArray<TVector>; const AUVs: TArray<TFloat2>; const AParts: TStringDynArray);
    class procedure NormalizeUVs(const AMesh: TMesh);
    class function IndexOfMaterial(const AMaterials: TArray<TMaterial>; const AName: string): Integer;
  public
    class function CanLoad(const AFileName: string): Boolean; override;
    class function LoadFromFile(const AFileName: string): TMeshGroup; override;
  end;

implementation

uses
  StrUtils,
  IOUtils;

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

class function TObjMeshLoader.IndexOfMaterial(
  const AMaterials: TArray<TMaterial>; const AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(AMaterials) do
    if AMaterials[i].Name = AName then
      Exit(i);
end;

class function TObjMeshLoader.LoadFromFile(const AFileName: string): TMeshGroup;
var
  LFile: TStringList;
  LLine: string;
  LParts: TStringDynArray;
  LFormat: TFormatSettings;
  LVertices: TArray<TVector>;
  LUVs: TArray<TFloat2>;
  LSubMesh: TMesh;
  LMaterials: TArray<TMaterial>;
  LMatIndex: Integer;
begin
  LFormat := TFormatSettings.Create();
  LFormat.DecimalSeparator := '.';
  Result := TMeshGroup.Create();
  LSubMesh := nil;
  LFile := TStringList.Create();
  try
    LFile.LoadFromFile(AFileName);
    for LLine in LFile do
    begin
      LParts := SplitString(Trim(LLine), ' ');
      if Length(LParts) > 0 then
      begin
        case AnsiIndexText(LParts[0], ['v', 'vt', 'vn', 'f', 'mtllib', 'usemtl']) of
          //vertex
          0: AddVertex(LVertices, LParts, LFormat);
          //UV coordinate
          1: AddUV(LUVs, LParts, LFormat);
          //normal
//          2:
          //face
          3:
          begin
            if not Assigned(LSubMesh) then
              LSubMesh := TMesh.Create();
            AddFace(LSubMesh, LVertices, LUVs, LParts);
          end;
          4: LMaterials := LoadMaterial(TPath.Combine(ExtractFilePath(AFileName), LParts[1]));
          5:
          begin
            if Assigned(LSubMesh) then
            begin
              if Assigned(LSubMesh.Vertices) then
              begin
                NormalizeUVs(LSubMesh);
                Result.Meshes.Add(LSubMesh)
              end
              else
                FreeAndNil(LSubMesh);
            end;
            LSubMesh := TMesh.Create();
            LMatIndex := IndexOfMaterial(LMaterials, LParts[1]);
            if LMatIndex > -1 then
              LSubMesh.Material := LMaterials[LMatIndex]
          end;
        end;
      end;
    end;
    if Assigned(LSubMesh) then
    begin
      if Assigned(LSubMesh.Vertices) then
        Result.Meshes.Add(LSubMesh)
      else
        LSubMesh.Free;
    end;
  finally
    LFile.Free;
  end;
end;

class function TObjMeshLoader.LoadMaterial(
  const AFileName: string): TArray<TMaterial>;
var
  LFile: TStringList;
  LParts: TStringDynArray;
  i: Integer;
  LMaterial: TMaterial;
begin
  Result := [];
  LMaterial := Default(TMaterial);
  if TFile.Exists(AFileName) then
  begin
    LFile := TStringList.Create();
    try
      LFile.LoadFromFile(AFileName);
      for i := 0 to Pred(LFile.Count) do
      begin
        LParts := SplitString(Trim(LFile[i]), ' ');
        if Length(LParts) > 1 then //expect at least 2 elements
        begin
          case AnsiIndexText(LParts[0], ['newmtl', 'map_kd']) of
            0:
            begin
              if LMaterial.Name <> '' then
                Result := Result + [LMaterial];
              LMaterial := Default(TMaterial);
              LMaterial.Name := LParts[1];
            end;
            1:
            begin
              LMaterial.Texture := ExtractFileName(LParts[1]);
            end;
          end;
        end;
      end;
      if LMaterial.Name <> '' then
        Result := Result + [LMaterial];
    finally
      LFile.Free;
    end;
  end;
end;

class procedure TObjMeshLoader.NormalizeUVs(const AMesh: TMesh);
var
  i: Integer;
  LLowest, LDiff, LUV: TFloat2;
begin
  if not Assigned(AMesh.UV) then Exit;

  LLowest := AMesh.UV[0];
  for i := 1 to High(AMesh.UV) do
  begin
    if AMesh.UV[i].U < LLowest.U then
      LLowest.U := AMesh.UV[i].U;
    if AMesh.UV[i].V < LLowest.V then
      LLowest.V := AMesh.UV[i].V;
  end;

  if (LLowest.U < 0) then
    LDiff.U := (1+Frac(LLowest.U)) - LLowest.U
  else
    LDiff.U := 0;

  if (LLowest.V < 0) then
    LDiff.V := (1+Frac(LLowest.V)) - LLowest.V
  else
    LDiff.V := 0;

  for i := Low(AMesh.UV) to High(AMesh.UV) do
  begin
    LUV := AMesh.UV[i];
    LUV.U := LUV.U + LDiff.U;
    LUV.V := LUV.V + LDiff.V;
    AMesh.UV[i] := LUV;
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
