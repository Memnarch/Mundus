unit Math3D;

interface

uses
  Types, Classes, SysUtils, Math;

type

  TFloatArray = array of Double;

  TMatrix = array of TFloatArray;

  TMatrixClass4D = class; //dummy

  TVectorClass3D = class
  private
    FElement: TFloatArray;
    function GetElement(Index: Integer): Double;
    procedure SetElement(Index: Integer; const Value: Double);
    function GetX: Double;
    function GetY: Double;
    function GetZ: Double;
    procedure SetX(const Value: Double);
    procedure SetY(const Value: Double);
    procedure SetZ(const Value: Double);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Clear(); virtual;
    procedure CopyFromVector3D(AVector: TVectorClass3D);
    procedure AddVector3D(AVector: TVectorClass3D);
    procedure SubtractVector3D(AVector: TVectorClass3D);
    procedure MultiplyVector3D(AFactor: Double);
    procedure CalculateSurfaceNormal(AVecA, AVecB, AVecC: TVectorClass3D);
    property Element[Index: Integer]: Double read GetElement write SetElement;
    property X: Double read GetX write SetX;
    property Y: Double read GetY write SetY;
    property Z: Double read GetZ write SetZ;
  end;

  TVectorClass4D = class(TVectorClass3D)
  private

  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    procedure Rescale(AKeepW: Boolean = False);
    procedure Clear(); override;
    procedure CopyFromVector4D(AVector: TVectorClass4D);
    procedure AddVector4D(AVector: TVectorClass4D);
    procedure SubtractVector4D(AVector: TVectorClass4D);
    procedure MultiplyVector4D(AFactor: Double);
    procedure MultiplyWithMatrix4D(AMatrix: TMatrixClass4D);
  end;

  TMatrixClass4D = class
  private
    FMatrix: TMatrix;
    function GetMatrixElement(IndexX, IndexY: Integer): Double;
    procedure SetMatrixElement(IndexX, IndexY: Integer; const Value: Double);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure SetAsNullMatrix4D();
    procedure SetAsNullMatrix3D();
    procedure SetAsIdentMatrix4D();
    procedure SetAsScaleMatrix(AX, AY, AZ: Double);
    procedure SetAsMoveMatrix(AX, AY, AZ: Double);
    procedure SetAsRotationXMatrix(AAlpha: Double);
    procedure SetAsRotationYMatrix(AAlpha: Double);
    procedure SetAsRotationZMatrix(AAlpha: Double);
    procedure SetAsPerspectiveProjectionMatrix(ZNear, ZFar, FOV, AspectRation: Double);
    procedure Clear();
    procedure CopyFromMatrix4D(AMatrix: TMatrixClass4D);
    procedure AddMatrix4D(AMatrix: TMatrixClass4D);
    procedure AddMatrix3D(AMatrix: TMatrixClass4D);
    procedure SubtractMatrix4D(AMatrix: TMatrixClass4D);
    procedure SubtractMatrix3D(AMatrix:TMatrixClass4D);
    procedure MultiplyMatrix4DWithFloat(AValue: Double);
    procedure MultiplyMatrix3DWithFloat(AValue: Double);
    procedure MultiplyMatrix4D(AMatrix: TMatrixClass4D);
    property Matrix[IndexX, IndexY: Integer]: Double read GetMatrixElement write SetMatrixElement;
  end;

implementation

{ TVectorClass3D }

procedure TVectorClass3D.AddVector3D(AVector: TVectorClass3D);
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    FElement[i] := FElement[i] + Avector.Element[i];
  end;
end;

procedure TVectorClass3D.CalculateSurfaceNormal(AVecA, AVecB,
  AVecC: TVectorClass3D);
var
  LVecU, LVecV: TVectorClass3D;
begin
  LVecU := TVectorClass3D.Create();
  LVecV := TVectorClass3D.Create();

  LVecU.CopyFromVector3D(AVecB);
  LVecU.SubtractVector3D(AVecA);

  LVecV.CopyFromVector3D(AVecC);
  LVecV.SubtractVector3D(AVecA);

  X := (LVecU.Y*LVecV.Z) - (LVecU.Z*LVecV.Y);
  Y := (LVecU.Z*LVecV.X) - (LVecU.X*LVecV.Z);
  Z := (LVecU.X*LVecV.Y) - (LVecU.Y*LVecV.X);
  LVecU.Free;
  LVecV.Free;
end;

procedure TVectorClass3D.Clear;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    Self.FElement[i] := 0;
  end;
end;

procedure TVectorClass3D.CopyFromVector3D(AVector: TVectorClass3D);
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    FElement[i] := AVector.Element[i];
  end;
end;

constructor TVectorClass3D.Create;
begin
  SetLength(FElement, 3);
  Clear();
end;

destructor TVectorClass3D.Destroy;
begin
  SetLength(FElement, 0);
  inherited;
end;

function TVectorClass3D.GetElement(Index: Integer): Double;
begin
  Result := FElement[Index];
end;

function TVectorClass3D.GetX: Double;
begin
  Result := Element[0];
end;

function TVectorClass3D.GetY: Double;
begin
  Result := Element[1];
end;

function TVectorClass3D.GetZ: Double;
begin
  Result := Element[2];
end;

procedure TVectorClass3D.MultiplyVector3D(AFactor: Double);
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    FElement[i] := FElement[i] * AFactor;
  end;
end;

procedure TVectorClass3D.SetElement(Index: Integer; const Value: Double);
begin
  FElement[Index] := Value;
end;

procedure TVectorClass3D.SetX(const Value: Double);
begin
  Element[0] := Value;
end;

procedure TVectorClass3D.SetY(const Value: Double);
begin
  Element[1] := Value;
end;

procedure TVectorClass3D.SetZ(const Value: Double);
begin
  Element[2] := Value;
end;

procedure TVectorClass3D.SubtractVector3D(AVector: TVectorClass3D);
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    FElement[i] := FElement[i] - Avector.Element[i];
  end;
end;

{ TVectorClass4D }

procedure TVectorClass4D.AddVector4D(AVector: TVectorClass4D);
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    FElement[i] := FElement[i] + AVector.Element[i];
  end;
end;

procedure TVectorClass4D.Clear;
begin
  inherited;
  FElement[3] := 1;
end;

procedure TVectorClass4D.CopyFromVector4D(AVector: TVectorClass4D);
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    FElement[i] := AVector.Element[i];
  end;
end;

constructor TVectorClass4D.Create;
begin
  SetLength(FElement, 4);
  Clear();
end;

destructor TVectorClass4D.Destroy;
begin
  SetLength(FElement, 0);
  inherited;
end;

procedure TVectorClass4D.MultiplyVector4D(AFactor: Double);
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    FElement[i] := FElement[i] * AFactor;
  end;
end;

procedure TVectorClass4D.MultiplyWithMatrix4D(AMatrix: TMatrixClass4D);
var
  i, k: Integer;
  LVector: TVectorClass4D;
begin
  LVector := TVectorClass4D.Create();

  for i := 0 to 3 do
  begin
    LVector.Element[i] := 0;
    for k := 0 to 3 do
    begin
      LVector.Element[i] := LVector.Element[i] + Amatrix.FMatrix[k, i] * Self.Element[k];
    end;
  end;
  Self.CopyFromVector4D(LVector);
//  if Self.Element[3] <> 1 then
//  begin
//    Self.Rescale();
//  end;
  LVector.Free();
end;

procedure TVectorClass4D.Rescale;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    Self.FElement[i] := Self.FElement[i] / Self.FElement[3];
  end;
  if not AKeepW then
  begin
    Self.FElement[3] := 1;
  end;
end;

procedure TVectorClass4D.SubtractVector4D(AVector: TVectorClass4D);
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    FElement[i] := FElement[i] - AVector.Element[i];
  end;
end;

{ TMatrixClass4D }

procedure TMatrixClass4D.AddMatrix3D(AMatrix: TMatrixClass4D);
var
  i, k: Integer;
begin
  for i := 0 to 3 do
  begin
    for k := 0 to 2 do
    begin
      FMatrix[i, k] := FMatrix[i, k] + AMatrix.FMatrix[i, k];
    end;
  end;
end;

procedure TMatrixClass4D.AddMatrix4D(AMatrix: TMatrixClass4D);
var
  i, k: Integer;
begin
  for i := 0 to 3 do
  begin
    for k := 0 to 3 do
    begin
      FMatrix[i, k] := FMatrix[i, k] + AMatrix.FMatrix[i, k];
    end;
  end;
end;

procedure TMatrixClass4D.Clear;
var
  i, k: Integer;
begin
  for i := 0 to 3 do
  begin
    for k := 0 to 3 do
    begin
      FMatrix[i, k] := 0;
    end;
  end;
end;

procedure TMatrixClass4D.CopyFromMatrix4D(AMatrix: TMatrixClass4D);
var
  i, k: Integer;
begin
  for i := 0 to 3 do
  begin
    for k := 0 to 3 do
    begin
      FMatrix[i, k] := AMatrix.FMatrix[i, k];
    end;
  end;

end;

constructor TMatrixClass4D.Create;
var
  i: Integer;
begin
  SetLength(FMatrix, 4);
  for i := 0 to 3 do
  begin
    SetLength(FMatrix[i], 4);
  end;
  Clear();
end;

destructor TMatrixClass4D.Destroy;
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    SetLength(FMatrix[i], 0);
  end;
  SetLength(FMatrix, 0);
  inherited;
end;



function TMatrixClass4D.GetMatrixElement(IndexX, IndexY: Integer): Double;
begin
  Result := FMatrix[IndexX, IndexY];
end;

procedure TMatrixClass4D.MultiplyMatrix3DWithFloat(AValue: Double);
var
  i, k: Integer;
begin
  for i := 0 to 3 do
  begin
    for k := 0 to 2 do
    begin
      FMatrix[i, k] := FMatrix[i, k] * AValue;
    end;
  end;
end;

procedure TMatrixClass4D.MultiplyMatrix4D(AMatrix: TMatrixClass4D);
var
  i, k, m: Integer;
  LMatrix: TMatrixClass4D;
begin
  LMatrix := TMatrixClass4D.Create();
//  LMatrix.CopyFromMatrix4D(Self);
  for i := 0 to 3 do
  begin
    for k := 0 to 3 do
    begin
      LMatrix.FMatrix[i, k] := 0;
      for m := 0 to 3 do
      begin
        LMatrix.FMatrix[i, k] := LMatrix.FMatrix[i, k] +  Self.FMatrix[m, k] * AMatrix.FMatrix[i, m];
      end;
    end;
  end;

  Self.CopyFromMatrix4D(LMatrix);
  LMatrix.Free();
end;

procedure TMatrixClass4D.MultiplyMatrix4DWithFloat(AValue: Double);
var
  i, k: Integer;
begin
  for i := 0 to 3 do
  begin
    for k := 0 to 3 do
    begin
      FMatrix[i, k] := FMatrix[i, k] * AValue;
    end;
  end;
end;


procedure TMatrixClass4D.SetAsIdentMatrix4D;
var
  i, k: Integer;
begin
  for i := 0 to 3 do
  begin
    for k := 0 to 3 do
    begin
      if i = k then
      begin
        FMatrix[i, k] := 1;
      end
      else
      begin
        FMatrix[i, k] := 0;
      end;
    end;
  end;
end;

procedure TMatrixClass4D.SetAsMoveMatrix(AX, AY, AZ: Double);
begin
  Clear();
  FMatrix[0, 0] := 1;
  FMatrix[1, 1] := 1;
  FMatrix[2, 2] := 1;
  FMatrix[3, 3] := 1;
  FMatrix[3, 0] := AX;
  FMatrix[3, 1] := AY;
  FMatrix[3, 2] := AZ;
end;

procedure TMatrixClass4D.SetAsNullMatrix3D;
begin
  Clear();
  FMatrix[3, 3] := 1;
end;

procedure TMatrixClass4D.SetAsNullMatrix4D;
begin
  Clear();
end;

procedure TMatrixClass4D.SetAsPerspectiveProjectionMatrix(ZNear, ZFar, FOV, AspectRation: Double);
begin
  Clear();
  FMatrix[0, 0] := Cotan(FOV/2)/AspectRation;// 2*ZNear/AspectRation;
  FMatrix[1, 1] := Cotan(FOV/2);
  FMatrix[2, 2] := ZFar / (ZFar - ZNear);
  FMatrix[3, 2] := -((ZFar*ZNear) / (ZFar - ZNear));
  FMatrix[2, 3] := 1;
end;

procedure TMatrixClass4D.SetAsRotationXMatrix(AAlpha: Double);
var
  LC, LS: Double;
begin
  Clear();
  FMatrix[3, 3] := 1;
  LC := cos(AAlpha);
  LS := sin(AAlpha);
  FMatrix[0][0] := 1;
  FMatrix[1][1] := LC;
  FMatrix[2][2] := LC;
  FMatrix[1][2] := LS;
  FMatrix[2][1] := -LS;
end;

procedure TMatrixClass4D.SetAsRotationYMatrix(AAlpha: Double);
var
  LC, LS: Double;
begin
  Clear();
  FMatrix[3, 3] := 1;
  LC := cos(AAlpha);
  LS := sin(AAlpha);
  FMatrix[1][1] := 1;
  FMatrix[0][0] := LC;
  FMatrix[2][2] := LC;
  FMatrix[0][2] := -LS;
  FMatrix[2][0] := LS;
end;

procedure TMatrixClass4D.SetAsRotationZMatrix(AAlpha: Double);
var
  LC, LS: Double;
begin
  Clear();
  FMatrix[3, 3] := 1;
  LC := cos(AAlpha);
  LS := sin(AAlpha);
  FMatrix[2][2] := 1;
  FMatrix[0][0] := LC;
  FMatrix[1][1] := LC;
  FMatrix[0][1] := LS;
  FMatrix[1][0] := -LS;
end;

procedure TMatrixClass4D.SetAsScaleMatrix(AX, AY, AZ: Double);
begin
  Clear;
  FMatrix[0, 0] := AX;
  FMatrix[1, 1] := AY;
  FMatrix[2, 2] := AZ;
  FMatrix[3, 3] := 1;
end;

procedure TMatrixClass4D.SetMatrixElement(IndexX, IndexY: Integer;
  const Value: Double);
begin
  FMatrix[IndexX, IndexY] := Value;
end;

procedure TMatrixClass4D.SubtractMatrix3D(AMatrix: TMatrixClass4D);
var
  i, k: Integer;
begin
  for i := 0 to 3 do
  begin
    for k := 0 to 2 do
    begin
      FMatrix[i, k] := FMatrix[i, k] + AMatrix.FMatrix[i, k];
    end;
  end;
end;

procedure TMatrixClass4D.SubtractMatrix4D(AMatrix: TMatrixClass4D);
var
  i, k: Integer;
begin
  for i := 0 to 3 do
  begin
    for k := 0 to 3 do
    begin
      FMatrix[i, k] := FMatrix[i, k] + AMatrix.FMatrix[i, k];
    end;
  end;
end;

end.
