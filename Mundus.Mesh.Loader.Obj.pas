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
  private type
    TMaterialInfo = record
      Material: TMaterial;
      DiffuseTexture: TTextureReference;
      class function Create: TMaterialInfo; static;
    end;
  private
    class function LoadMaterial(const AFileName: string; AMesh: TMesh): TArray<TMaterialInfo>;
    class function ParseFace(const AText: string; AVertexCount, ANormalCount, AUVCount: Integer): TFacePoint;
    class procedure AddVector(var ATarget: TArray<TVector>; const AParts: TStringDynArray; const AFormat: TFormatSettings);
    class procedure AddUV(var AUVs: TArray<TFloat2>; const AParts: TStringDynArray; const AFormat: TFormatSettings);
    class procedure AddFace(const AMesh: TMesh; const AVertices, ANormals: TArray<TVector>; const AUVs: TArray<TFloat2>; const AParts: TStringDynArray);
    class procedure NormalizeUVs(const AMesh: TMesh);
    class function IndexOfMaterial(const AMaterials: TArray<TMaterialInfo>; const AName: string): Integer;
  public
    class function CanLoad(const AFileName: string): Boolean; override;
    class function LoadFromFile(const AFileName: string): TMeshGroup; override;
  end;

implementation

uses
  StrUtils,
  IOUtils;

const
  CNoIndex = Low(Integer);

{ TObjMeshLoader }

class procedure TObjMeshLoader.AddFace(const AMesh: TMesh;
  const AVertices, ANormals: TArray<TVector>; const AUVs: TArray<TFloat2>;
  const AParts: TStringDynArray);
var
  LPoints: TArray<TFacePoint>;
  LIndices: TArray<Integer>;
  i: Integer;
begin
  SetLength(LPoints, Length(AParts) - 1);
  for i := 0 to High(LPoints) do
    LPoints[i] := ParseFace(AParts[i+1], Length(AVertices), Length(ANormals), Length(AUVs));

  SetLength(LIndices, Length(LPoints));
  for i := 0 to High(LPoints) do
  begin
    LIndices[i] := AMesh.AddVertice(AVertices[LPoints[i].VIndex]);
    if LPoints[i].UVIndex > -1 then
      AMesh.AddUV(AUVs[LPoints[i].UVIndex]);
    if LPoints[i].NIndex > -1 then
      AMesh.AddNormal(ANormals[LPoints[i].NIndex]);
  end;

  if Length(LIndices) > 2 then
    AMesh.AddIndices([LIndices[0], LIndices[1], LIndices[2]]);

  if Length(LIndices) > 3 then
    AMesh.AddIndices([LIndices[0], LIndices[2], LIndices[3]]);
end;

class procedure TObjMeshLoader.AddUV(var AUVs: TArray<TFloat2>; const AParts: TStringDynArray; const AFormat: TFormatSettings);
begin
  AUVs := AUVs + [TFloat2.Create(StrToFloatDef(AParts[1], 0, AFormat), StrToFloatDef(AParts[2], 0, AFormat))]
end;

class procedure TObjMeshLoader.AddVector(var ATarget: TArray<TVector>; const AParts: TStringDynArray; const AFormat: TFormatSettings);
begin
  ATarget := ATarget + [Vector(StrToFloatDef(AParts[1], 0, AFormat), StrToFloatDef(AParts[2], 0, AFormat), StrToFloatDef(AParts[3], 0, AFormat))];
end;

class function TObjMeshLoader.CanLoad(const AFileName: string): Boolean;
begin
  Result := AnsiSameText(ExtractFileExt(AFileName), '.obj');
end;

class function TObjMeshLoader.IndexOfMaterial(const AMaterials: TArray<TMaterialInfo>; const AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(AMaterials) do
    if AMaterials[i].Material.Name = AName then
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
  LNormals: TArray<TVector>;
  LSubMesh: TMesh;
  LMaterialInfos: TArray<TMaterialInfo>;
  LInfoReference: ^TMaterialInfo;
  LMaterialReference: ^TMaterial;
  LMatIndex: Integer;
  LTrimedLine, LFilePath: string;
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
      LTrimedLine := Trim(LLine);
      LParts := SplitString(LTrimedLine, ' ');
      if Length(LParts) > 0 then
      begin
        case AnsiIndexText(LParts[0], ['v', 'vt', 'vn', 'f', 'mtllib', 'usemtl']) of
          //vertex
          0: AddVector(LVertices, LParts, LFormat);
          //UV coordinate
          1: AddUV(LUVs, LParts, LFormat);
          //normal
          2: AddVector(LNormals, LParts, LFormat);
          //face
          3:
          begin
            if not Assigned(LSubMesh) then
              LSubMesh := TMesh.Create();
            AddFace(LSubMesh, LVertices, LNormals, LUVs, LParts);
          end;
          4:
          begin
            if not Assigned(LSubMesh) then
              LSubMesh := TMesh.Create();
            LFilePath := Copy(LTrimedLine, Length('mtllib ') + 1);
            LMaterialInfos := LoadMaterial(TPath.Combine(ExtractFilePath(AFileName), ExtractFileName(LFilePath)), LSubMesh);
          end;
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
            LMatIndex := IndexOfMaterial(LMaterialInfos, LParts[1]);
            if LMatIndex > -1 then
            begin
              LInfoReference := @LMaterialInfos[LMatIndex];
              LMaterialReference := @LSubMesh.Material;
              LMaterialReference^ := LInfoReference.Material;
              if LInfoReference.DiffuseTexture.Name <> '' then
                LMaterialReference.DiffuseTexture := LSubMesh.AddTextureReference(LInfoReference.DiffuseTexture);
            end;
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

class function TObjMeshLoader.LoadMaterial(const AFileName: string; AMesh: TMesh): TArray<TMaterialInfo>;
var
  LFile: TStringList;
  LParts: TStringDynArray;
  i: Integer;
  LInfo: TMaterialInfo;
begin
  Result := [];
  LInfo := TMaterialInfo.Create();
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
              if LInfo.Material.Name <> '' then
                Result := Result + [LInfo];
              LInfo := TMaterialInfo.Create();
              LInfo.Material.Name := LParts[1];
            end;
            1:
            begin
              LInfo.DiffuseTexture.FileName := ExtractFileName(LParts[1]);
              LInfo.DiffuseTexture.Name := ChangeFileExt(LInfo.DiffuseTexture.FileName, '');
            end;
          end;
        end;
      end;
      if LInfo.Material.Name <> '' then
        Result := Result + [LInfo];
    finally
      LFile.Free;
    end;
  end;
end;

class procedure TObjMeshLoader.NormalizeUVs(const AMesh: TMesh);
var
  i: Integer;
  LLowest, LDiff, LUV: TFloat2;
  LUVSet: TArray<TFloat2>;
begin
  if not Assigned(AMesh.UVs) then Exit;

  LUVSet := AMesh.UVs[0];

  LLowest := LUVSet[0];
  for i := 1 to High(LUVSet) do
  begin
    if LUVSet[i].U < LLowest.U then
      LLowest.U := LUVSet[i].U;
    if LUVSet[i].V < LLowest.V then
      LLowest.V := LUVSet[i].V;
  end;

  if (LLowest.U < 0) then
    LDiff.U := (1+Frac(LLowest.U)) - LLowest.U
  else
    LDiff.U := 0;

  if (LLowest.V < 0) then
    LDiff.V := (1+Frac(LLowest.V)) - LLowest.V
  else
    LDiff.V := 0;

  for i := Low(LUVSet) to High(LUVSet) do
  begin
    LUV := LUVSet[i];
    LUV.U := LUV.U + LDiff.U;
    LUV.V := LUV.V + LDiff.V;
    LUVSet[i] := LUV;
  end;
end;

function NormalizeIndex(const AIndex, AItemCount: Integer): Integer;
begin
  if AIndex < 0 then
    Result := AItemCount + AIndex
  else
    Result := AIndex - 1;
end;

class function TObjMeshLoader.ParseFace(const AText: string; AVertexCount, ANormalCount, AUVCount: Integer): TFacePoint;
var
  LParts: TStringDynArray;
begin
  LParts := SplitString(AText, '/');
  Result.VIndex := NormalizeIndex(StrToIntDef(LParts[0], 1), AVertexCount);
  if Length(LParts) > 1 then
    Result.UVIndex := NormalizeIndex(StrToIntDef(LParts[1], 1), AUVCount)
  else
    Result.UVIndex := -1;

  if Length(LParts) > 2 then
    Result.NIndex := NormalizeIndex(StrToIntDef(LParts[2], 1), ANormalCount)
  else
    Result.NIndex := -1;
end;

{ TObjMeshLoader.TMaterialInfo }

class function TObjMeshLoader.TMaterialInfo.Create: TMaterialInfo;
begin
  Result.Material := TMaterial.Create;
end;

initialization
  TMeshLoaders.RegisterLoader(TObjMeshLoader);

end.
