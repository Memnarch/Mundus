unit Mundus.Math;

interface

uses
  Types, Classes, SysUtils, Math;

type
  TInt4 = record
    X, Y, Z, W: Integer;
    procedure Add(const ARight: TInt4);
    procedure Sub(const ARight: TInt4);
    procedure Mul(const ARight: TInt4);
  end;

  TFloatArray = TArray<Single>;

  TFloat2 = record
    constructor Create(AX, AY: Single);
    case byte of
      0: (X, Y: Single);
      1: (U, V: Single);
      2: (Element: array[0..1] of Single);
  end;

  TFloat3 = record
    X, Y, Z: Single;
    procedure Add(const ARight: TFloat3);
    procedure Sub(const ARight: TFloat3);
    procedure Mul(const AFactor: Single); overload;
    procedure Mul(const ARight: TFloat3); overload;
    procedure CalculateSurfaceNormal(const AVecA, AVecB, AVecC: TFloat3);
  end;

  TFloat4 = record
    procedure Add(const ARight: TFloat4);
    procedure Sub(const ARight: TFloat4);
    procedure Mul(const AFactor: Single); overload;
    procedure Mul(const ARight: TFloat4); overload;
    procedure Dot(const ARight: TFloat4); overload;
    procedure Cross(const ARight: TFloat4); overload;
    procedure CalculateSurfaceNormal(const AVecA, AVecB, AVecC: TFloat4);
    procedure Normalize;
    procedure NormalizeKeepW;
    function Length: Single;
    case byte of
      0: (X, Y, Z, W: Single);
      1: (XY, ZW: TFloat2);
      2: (Element: array[0..3] of Single);
      3: (B, G, R, A: Single);
  end;

  TMatrix4D = record
    Values: array[0..3] of TFloat4;
  end;

  PVector4D = ^TFloat4;

  TMatrix4x4 = record
  private
    FMatrix: array[0..3] of array[0..3] of Single;
    function GetMatrixElement(IndexX, IndexY: Integer): Single;
    procedure SetMatrixElement(IndexX, IndexY: Integer; const Value: Single);
  public
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
    procedure AddMatrix4D(AMatrix: TMatrix4x4);
    procedure AddMatrix3D(AMatrix: TMatrix4x4);
    procedure SubtractMatrix4D(AMatrix: TMatrix4x4);
    procedure SubtractMatrix3D(AMatrix:TMatrix4x4);
    procedure MultiplyMatrix4DWithFloat(AValue: Double);
    procedure MultiplyMatrix3DWithFloat(AValue: Double);
    procedure MultiplyMatrix4D(AMatrix: TMatrix4x4);
    function Transform(const AVector: TFloat4): TFloat4;
    function Inverse: TMatrix4x4;
    property Matrix[IndexX, IndexY: Integer]: Single read GetMatrixElement write SetMatrixElement;
  end;

function Float3(X, Y, Z: Single): TFloat3;
function Float4(X, Y, Z, W: Single): TFloat4;

implementation

function Float3(X, Y, Z: Single): TFloat3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Float4(X, Y, Z, W: Single): TFloat4;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

{ TMatrix4x4 }

procedure TMatrix4x4.AddMatrix3D(AMatrix: TMatrix4x4);
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

procedure TMatrix4x4.AddMatrix4D(AMatrix: TMatrix4x4);
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

procedure TMatrix4x4.Clear;
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

function TMatrix4x4.GetMatrixElement(IndexX, IndexY: Integer): Single;
begin
  Result := FMatrix[IndexX, IndexY];
end;

//Presented by the lovely folks of Stackoverflow
//https://stackoverflow.com/a/44446912
{$Region CCode}
{
var A2323 = m.m22 * m.m33 - m.m23 * m.m32 ;
var A1323 = m.m21 * m.m33 - m.m23 * m.m31 ;
var A1223 = m.m21 * m.m32 - m.m22 * m.m31 ;
var A0323 = m.m20 * m.m33 - m.m23 * m.m30 ;
var A0223 = m.m20 * m.m32 - m.m22 * m.m30 ;
var A0123 = m.m20 * m.m31 - m.m21 * m.m30 ;
var A2313 = m.m12 * m.m33 - m.m13 * m.m32 ;
var A1313 = m.m11 * m.m33 - m.m13 * m.m31 ;
var A1213 = m.m11 * m.m32 - m.m12 * m.m31 ;
var A2312 = m.m12 * m.m23 - m.m13 * m.m22 ;
var A1312 = m.m11 * m.m23 - m.m13 * m.m21 ;
var A1212 = m.m11 * m.m22 - m.m12 * m.m21 ;
var A0313 = m.m10 * m.m33 - m.m13 * m.m30 ;
var A0213 = m.m10 * m.m32 - m.m12 * m.m30 ;
var A0312 = m.m10 * m.m23 - m.m13 * m.m20 ;
var A0212 = m.m10 * m.m22 - m.m12 * m.m20 ;
var A0113 = m.m10 * m.m31 - m.m11 * m.m30 ;
var A0112 = m.m10 * m.m21 - m.m11 * m.m20 ;

var det = m.m00 * ( m.m11 * A2323 - m.m12 * A1323 + m.m13 * A1223 )
    - m.m01 * ( m.m10 * A2323 - m.m12 * A0323 + m.m13 * A0223 )
    + m.m02 * ( m.m10 * A1323 - m.m11 * A0323 + m.m13 * A0123 )
    - m.m03 * ( m.m10 * A1223 - m.m11 * A0223 + m.m12 * A0123 ) ;
det = 1 / det;

return new Matrix4x4() {
   m00 = det *   ( m.m11 * A2323 - m.m12 * A1323 + m.m13 * A1223 ),
   m01 = det * - ( m.m01 * A2323 - m.m02 * A1323 + m.m03 * A1223 ),
   m02 = det *   ( m.m01 * A2313 - m.m02 * A1313 + m.m03 * A1213 ),
   m03 = det * - ( m.m01 * A2312 - m.m02 * A1312 + m.m03 * A1212 ),
   m10 = det * - ( m.m10 * A2323 - m.m12 * A0323 + m.m13 * A0223 ),
   m11 = det *   ( m.m00 * A2323 - m.m02 * A0323 + m.m03 * A0223 ),
   m12 = det * - ( m.m00 * A2313 - m.m02 * A0313 + m.m03 * A0213 ),
   m13 = det *   ( m.m00 * A2312 - m.m02 * A0312 + m.m03 * A0212 ),
   m20 = det *   ( m.m10 * A1323 - m.m11 * A0323 + m.m13 * A0123 ),
   m21 = det * - ( m.m00 * A1323 - m.m01 * A0323 + m.m03 * A0123 ),
   m22 = det *   ( m.m00 * A1313 - m.m01 * A0313 + m.m03 * A0113 ),
   m23 = det * - ( m.m00 * A1312 - m.m01 * A0312 + m.m03 * A0112 ),
   m30 = det * - ( m.m10 * A1223 - m.m11 * A0223 + m.m12 * A0123 ),
   m31 = det *   ( m.m00 * A1223 - m.m01 * A0223 + m.m02 * A0123 ),
   m32 = det * - ( m.m00 * A1213 - m.m01 * A0213 + m.m02 * A0113 ),
   m33 = det *   ( m.m00 * A1212 - m.m01 * A0212 + m.m02 * A0112 ),
}//;

{$endregion}
function TMatrix4x4.Inverse: TMatrix4x4;
var
  A2323,
  A1323,
  A1223,
  A0323,
  A0223,
  A0123,
  A2313,
  A1313,
  A1213,
  A2312,
  A1312,
  A1212,
  A0313,
  A0213,
  A0312,
  A0212,
  A0113,
  A0112,
  det: Single;
begin
  A2323 := FMatrix[2,2] * FMatrix[3,3] - FMatrix[2,3] * FMatrix[3,2];
  A1323 := FMatrix[2,1] * FMatrix[3,3] - FMatrix[2,3] * FMatrix[3,1];
  A1223 := FMatrix[2,1] * FMatrix[3,2] - FMatrix[2,2] * FMatrix[3,1];
  A0323 := FMatrix[2,0] * FMatrix[3,3] - FMatrix[2,3] * FMatrix[3,0] ;
  A0223 := FMatrix[2,0] * FMatrix[3,2] - FMatrix[2,2] * FMatrix[3,0] ;
  A0123 := FMatrix[2,0] * FMatrix[3,1] - FMatrix[2,1] * FMatrix[3,0] ;
  A2313 := FMatrix[1,2] * FMatrix[3,3] - FMatrix[1,3] * FMatrix[3,2] ;
  A1313 := FMatrix[1,1] * FMatrix[3,3] - FMatrix[1,3] * FMatrix[3,1] ;
  A1213 := FMatrix[1,1] * FMatrix[3,2] - FMatrix[1,2] * FMatrix[3,1] ;
  A2312 := FMatrix[1,2] * FMatrix[2,3] - FMatrix[1,3] * FMatrix[2,2] ;
  A1312 := FMatrix[1,1] * FMatrix[2,3] - FMatrix[1,3] * FMatrix[2,1] ;
  A1212 := FMatrix[1,1] * FMatrix[2,2] - FMatrix[1,2] * FMatrix[2,1] ;
  A0313 := FMatrix[1,0] * FMatrix[3,3] - FMatrix[1,3] * FMatrix[3,0] ;
  A0213 := FMatrix[1,0] * FMatrix[3,2] - FMatrix[1,2] * FMatrix[3,0] ;
  A0312 := FMatrix[1,0] * FMatrix[2,3] - FMatrix[1,3] * FMatrix[2,0] ;
  A0212 := FMatrix[1,0] * FMatrix[2,2] - FMatrix[1,2] * FMatrix[2,0] ;
  A0113 := FMatrix[1,0] * FMatrix[3,1] - FMatrix[1,1] * FMatrix[3,0] ;
  A0112 := FMatrix[1,0] * FMatrix[2,1] - FMatrix[1,1] * FMatrix[2,0] ;

  det := FMatrix[0,0] * ( FMatrix[1,1] * A2323 - FMatrix[1,2] * A1323 + FMatrix[1,3] * A1223 )
    - FMatrix[0,1] * ( FMatrix[1,0] * A2323 - FMatrix[1,2] * A0323 + FMatrix[1,3] * A0223 )
    + FMatrix[0,2] * ( FMatrix[1,0] * A1323 - FMatrix[1,1] * A0323 + FMatrix[1,3] * A0123 )
    - FMatrix[0,3] * ( FMatrix[1,0] * A1223 - FMatrix[1,1] * A0223 + FMatrix[1,2] * A0123 ) ;
  det := 1 / det;

  Result.FMatrix[0,0] := det *   ( FMatrix[1,1] * A2323 - FMatrix[1,2] * A1323 + FMatrix[1,3] * A1223 );
  Result.FMatrix[0,1] := det * - ( FMatrix[0,1] * A2323 - FMatrix[0,2] * A1323 + FMatrix[0,3] * A1223 );
  Result.FMatrix[0,2] := det *   ( FMatrix[0,1] * A2313 - FMatrix[0,2] * A1313 + FMatrix[0,3] * A1213 );
  Result.FMatrix[0,3] := det * - ( FMatrix[0,1] * A2312 - FMatrix[0,2] * A1312 + FMatrix[0,3] * A1212 );
  Result.FMatrix[1,0] := det * - ( FMatrix[1,0] * A2323 - FMatrix[1,2] * A0323 + FMatrix[1,3] * A0223 );
  Result.FMatrix[1,1] := det *   ( FMatrix[0,0] * A2323 - FMatrix[0,2] * A0323 + FMatrix[0,3] * A0223 );
  Result.FMatrix[1,2] := det * - ( FMatrix[0,0] * A2313 - FMatrix[0,2] * A0313 + FMatrix[0,3] * A0213 );
  Result.FMatrix[1,3] := det *   ( FMatrix[0,0] * A2312 - FMatrix[0,2] * A0312 + FMatrix[0,3] * A0212 );
  Result.FMatrix[2,0] := det *   ( FMatrix[1,0] * A1323 - FMatrix[1,1] * A0323 + FMatrix[1,3] * A0123 );
  Result.FMatrix[2,1] := det * - ( FMatrix[0,0] * A1323 - FMatrix[0,1] * A0323 + FMatrix[0,3] * A0123 );
  Result.FMatrix[2,2] := det *   ( FMatrix[0,0] * A1313 - FMatrix[0,1] * A0313 + FMatrix[0,3] * A0113 );
  Result.FMatrix[2,3] := det * - ( FMatrix[0,0] * A1312 - FMatrix[0,1] * A0312 + FMatrix[0,3] * A0112 );
  Result.FMatrix[3,0] := det * - ( FMatrix[1,0] * A1223 - FMatrix[1,1] * A0223 + FMatrix[1,2] * A0123 );
  Result.FMatrix[3,1] := det *   ( FMatrix[0,0] * A1223 - FMatrix[0,1] * A0223 + FMatrix[0,2] * A0123 );
  Result.FMatrix[3,2] := det * - ( FMatrix[0,0] * A1213 - FMatrix[0,1] * A0213 + FMatrix[0,2] * A0113 );
  Result.FMatrix[3,3] := det *   ( FMatrix[0,0] * A1212 - FMatrix[0,1] * A0212 + FMatrix[0,2] * A0112 );
end;

procedure TMatrix4x4.MultiplyMatrix3DWithFloat(AValue: Double);
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

procedure TMatrix4x4.MultiplyMatrix4D(AMatrix: TMatrix4x4);
var
  i, k, m: Integer;
  LMatrix: TMatrix4x4;
begin
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

  Self := LMatrix;
end;

procedure TMatrix4x4.MultiplyMatrix4DWithFloat(AValue: Double);
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


procedure TMatrix4x4.SetAsIdentMatrix4D;
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

procedure TMatrix4x4.SetAsMoveMatrix(AX, AY, AZ: Double);
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

procedure TMatrix4x4.SetAsNullMatrix3D;
begin
  Clear();
  FMatrix[3, 3] := 1;
end;

procedure TMatrix4x4.SetAsNullMatrix4D;
begin
  Clear();
end;

procedure TMatrix4x4.SetAsPerspectiveProjectionMatrix(ZNear, ZFar, FOV, AspectRation: Double);
begin
  Clear();
  FMatrix[0, 0] := Cotan(FOV/2)/AspectRation;// 2*ZNear/AspectRation;
  FMatrix[1, 1] := Cotan(FOV/2);
  FMatrix[2, 2] := ZFar / (ZFar - ZNear);
  FMatrix[3, 2] := -((ZFar*ZNear) / (ZFar - ZNear));
  FMatrix[2, 3] := 1;
end;

procedure TMatrix4x4.SetAsRotationXMatrix(AAlpha: Double);
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

procedure TMatrix4x4.SetAsRotationYMatrix(AAlpha: Double);
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

procedure TMatrix4x4.SetAsRotationZMatrix(AAlpha: Double);
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

procedure TMatrix4x4.SetAsScaleMatrix(AX, AY, AZ: Double);
begin
  Clear;
  FMatrix[0, 0] := AX;
  FMatrix[1, 1] := AY;
  FMatrix[2, 2] := AZ;
  FMatrix[3, 3] := 1;
end;

procedure TMatrix4x4.SetMatrixElement(IndexX, IndexY: Integer;
  const Value: Single);
begin
  FMatrix[IndexX, IndexY] := Value;
end;

procedure TMatrix4x4.SubtractMatrix3D(AMatrix: TMatrix4x4);
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

procedure TMatrix4x4.SubtractMatrix4D(AMatrix: TMatrix4x4);
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

function TMatrix4x4.Transform(const AVector: TFloat4): TFloat4;
var
  i, k: Integer;
begin
  for i := 0 to 3 do
  begin
    Result.Element[i] := 0;
    for k := 0 to 3 do
    begin
      Result.Element[i] := Result.Element[i] + FMatrix[k, i] * AVector.Element[k];
    end;
  end;
end;

{ TFloat4 }

procedure TFloat4.Add(const ARight: TFloat4);
asm
  movups xmm0, [Self]
  movups xmm1, [ARight]
  addps xmm0, xmm1
  movups [Self], xmm0
end;
//begin
//  X := X + ARight.X;
//  Y := Y + ARight.Y;
//  Z := Z + ARight.Z;
//  W := W + ARight.W;
//end;

procedure TFloat4.Mul(const AFactor: Single);
asm
  //load factor into lowest part of xmm1
  movss xmm0, [AFactor]
//  //set all parts of xmm1 to the value in the lowest part of xmm1
  shufps xmm0, xmm0, 0
  movups xmm1, [Self]
  mulps xmm0, xmm1
  movups [Self], xmm0
end;

procedure TFloat4.CalculateSurfaceNormal(const AVecA, AVecB, AVecC: TFloat4);
var
  LVecU, LVecV: TFloat4;
begin
  LVecU := AVecB;
  LVecU.Sub(AVecA);

  LVecV := AVecC;
  LVecV.Sub(AVecA);

  LVecU.Cross(LVecV);
  Self := LVecU;
//  X := (LVecU.Y*LVecV.Z) - (LVecU.Z*LVecV.Y);
//  Y := (LVecU.Z*LVecV.X) - (LVecU.X*LVecV.Z);
//  Z := (LVecU.X*LVecV.Y) - (LVecU.Y*LVecV.X);
end;

procedure TFloat4.Cross(const ARight: TFloat4);
asm
  movups xmm0, [Self]
  movups xmm1, [ARight]
  //vshufps xmm3, xmm1, xmm1, 201
  movaps xmm3, xmm1
  shufps xmm3, xmm3, 201

  //vshufps xmm2, xmm0, xmm0
  movaps xmm2, xmm0
  shufps xmm2, xmm0, 201
  mulps xmm0, xmm3
  mulps xmm1, xmm2
  subps xmm0, xmm1
  shufps xmm0, xmm0, 201
  movups [Self], xmm0
end;

procedure TFloat4.Dot(const ARight: TFloat4);
asm
  movups xmm0, [Self]
  movups xmm1, [ARight]
  dpps xmm0, xmm1, $FF
  movups [Self], xmm0
end;

function TFloat4.Length: Single;
begin
  Result := Sqrt(X*X+Y*Y+Z*Z);
end;

procedure TFloat4.Mul(const ARight: TFloat4);
asm
  movups xmm0, [Self]
  movups xmm1, [ARight]
  mulps xmm0, xmm1
  movups [Self], xmm0
end;

procedure TFloat4.Normalize;
asm
  movups xmm0, [Self]
  movaps xmm1, xmm0
  //move highest (w) into all elements
  shufps xmm1, xmm1, $FF//0//$FF
  divps xmm0, xmm1
  movups [Self], xmm0
end;

procedure TFloat4.NormalizeKeepW;
var
  LW: Single;
begin
  LW := W;
  Normalize;
  W := LW;
end;

procedure TFloat4.Sub(const ARight: TFloat4);
asm
  movups xmm0, [Self]
  movups xmm1, [ARight]
  subps xmm0, xmm1
  movups [Self], xmm0
end;
//begin
//  X := X - ARight.X;
//  Y := Y - ARight.Y;
//  Z := Z - ARight.Z;
//  W := W - ARight.W;
//end;

{ TFloat3 }

procedure TFloat3.Add(const ARight: TFloat3);
asm
  //load xy
  movq xmm0, [Self]
  //load z
  insertps xmm0, [Self.z], 3 shl 4
  //load xy
  movq xmm1, [ARight]
  //load z
  insertps xmm1, [ARight.Z], 3 shl 4
  addps xmm0, xmm1
  //extract z
  extractps [Self.Z], xmm0, 3 shl 0
  //extract xy
  movq [Self], xmm0
//begin
//  X := X + ARight.X;
//  Y := Y + ARight.Y;
//  Z := Z + ARight.Z;
end;

procedure TFloat3.Mul(const AFactor: Single);
asm
  //load xy
  movq xmm0, [Self]
  //load z
  insertps xmm0, [Self.z], 3 shl 4

  //load factor into lowest part of xmm1
  movss xmm1, [AFactor]
  //set all parts of xmm1 to the value in the lowest part of xmm1
  shufps xmm1, xmm1, 0

  mulps xmm0, xmm1
  //extract z
  extractps [Self.Z], xmm0, 3 shl 0
  //extract xy
  movq [Self], xmm0
//begin
//  X := X * AFactor;
//  Y := Y * AFactor;
//  Z := Z * AFactor;
end;

procedure TFloat3.CalculateSurfaceNormal(const AVecA, AVecB, AVecC: TFloat3);
var
  LVecU, LVecV: TFloat3;
begin
  LVecU := AVecB;
  LVecU.Sub(AVecA);

  LVecV := AVecC;
  LVecV.Sub(AVecA);
  X := (LVecU.Y*LVecV.Z) - (LVecU.Z*LVecV.Y);
  Y := (LVecU.Z*LVecV.X) - (LVecU.X*LVecV.Z);
  Z := (LVecU.X*LVecV.Y) - (LVecU.Y*LVecV.X);
end;

procedure TFloat3.Mul(const ARight: TFloat3);
asm
  //load xy
  movq xmm0, [Self]
  //load z
  insertps xmm0, [Self.z], 3 shl 4

  //load xy
  movq xmm1, [ARight]
  //load z
  insertps xmm1, [ARight.z], 3 shl 4

  mulps xmm0, xmm1
  //extract z
  extractps [Self.Z], xmm0, 3 shl 0
  //extract xy
  movq [Self], xmm0
end;

procedure TFloat3.Sub(const ARight: TFloat3);
asm
  //load xy
  movq xmm0, [Self]
  //load z
  insertps xmm0, [Self.z], 3 shl 4
  //load xy
  movq xmm1, [ARight]
  //load z
  insertps xmm1, [ARight.Z], 3 shl 4

  subps xmm0, xmm1
  //extract z
  extractps [Self.Z], xmm0, 3 shl 0
  //extract xy
  movq [Self], xmm0
//begin
//  X := X - ARight.X;
//  Y := Y - ARight.Y;
//  Z := Z - ARight.Z;
end;

{ TInt4 }

procedure TInt4.Add(const ARight: TInt4);
asm
  movdqu xmm0, [Self]
  movdqu xmm1, [ARight]
  PADDD xmm0, xmm1
  movdqu [Self], xmm0
end;

procedure TInt4.Mul(const ARight: TInt4);
asm
  movdqu xmm0, [Self]
  movdqu xmm1, [ARight]
  PMULLD xmm0, xmm1
  movdqu [Self], xmm0
end;

procedure TInt4.Sub(const ARight: TInt4);
asm
  movdqu xmm0, [Self]
  movdqu xmm1, [ARight]
  PSUBD xmm0, xmm1
  movdqu [Self], xmm0
end;

{ TFloat2 }

constructor TFloat2.Create(AX, AY: Single);
begin
  X := AX;
  Y := AY;
end;

end.
