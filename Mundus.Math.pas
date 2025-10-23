unit Mundus.Math;

interface

type
  TFloat2 = record
    constructor Create(AX, AY: Single);
    class operator Add(const ALeft, ARight: TFloat2): TFloat2; static;
    class operator Subtract(const ALeft, ARight: TFloat2): TFloat2; static;
    class operator Multiply(const ALeft, ARight: TFloat2): TFloat2; static;
    class operator Multiply(const ALeft: TFloat2; AValue: Single): TFloat2; static;
    class operator Divide(const ALeft, ARight: TFloat2): TFloat2; static;
    class operator Divide(const ALeft: TFloat2; AValue: Single): TFloat2; static;
    function Length: Single;
    case byte of
      0: (X, Y: Single);
      1: (U, V: Single);
      2: (Elements: array[0..1] of Single);
  end;

  TFloat3 = record
    class operator Add(const ALeft, ARight: TFloat3): TFloat3; static;
    class operator Subtract(const ALeft, ARight: TFloat3): TFloat3; static;
    class operator Multiply(const ALeft, ARight: TFloat3): TFloat3; static;
    class operator Multiply(const ALeft: TFloat3; AValue: Single): TFloat3; static;
    class operator Divide(const ALeft, ARight: TFloat3): TFloat3; static;
    class operator Divide(const ALeft: TFloat3; AValue: Single): TFloat3; static;
    function Length: Single;
    case byte of
    0: (X, Y, Z: Single);
    1: (XY: TFloat2);
    2: (UV: TFloat2);
    3: (B, G, R: Single);
    4: (Elements: array[0..2] of Single);
  end;

  TFloat4 = packed record
    class operator Add(const ALeft, ARight: TFloat4): TFloat4; static;
    class operator Subtract(const ALeft, ARight: TFloat4): TFloat4; static;
    class operator Multiply(const ALeft, ARight: TFloat4): TFloat4; static;
    class operator Multiply(const ALeft: TFloat4; AValue: Single): TFloat4; static;
    class operator Divide(const ALeft, ARight: TFloat4): TFloat4; static;
    class operator Divide(const ALeft: TFloat4; AValue: Single): TFloat4; static;
    function Length: Single;
    case byte of
      0: (X, Y, Z, W: Single);
      1: (XY, ZW: TFloat2);
      2: (XYZ: TFloat3);
      3: (Elements: array[0..3] of Single);
      4: (B, G, R, A: Single);
      5: (BGR: TFloat3);
  end;

  TMatrix4x4 = record
  public
    class function CreateNullMatrix: TMatrix4x4; static;
    class function CreateIdentityMatrix: TMatrix4x4; static;
    class function CreatePerspectiveProjectionMatrix(ZNear, ZFar, FOV, AspectRatio: Single): TMatrix4x4; static;
    class function CreateScaleMatrix(X, Y, Z: Single): TMatrix4x4; static;
    class function CreateTranslationMatrix(X, Y, Z: Single): TMatrix4x4; static;
    class function CreateRotationXMatrix(DegAlpha: Single): TMatrix4x4; static;
    class function CreateRotationYMatrix(DegAlpha: Single): TMatrix4x4; static;
    class function CreateRotationZMatrix(DegAlpha: Single): TMatrix4x4; static;
    class operator Multiply(const ALeft, ARight: TMatrix4x4): TMatrix4x4; static;
    class operator Multiply(const ALeft: TMatrix4x4; const ARight: TFloat4): TFloat4; static;
    function Inverse: TMatrix4x4;
  private
    case byte of
      0: (FItems: array[0..3, 0..3] of Single);
      1: (FRows: array[0..3] of TFloat4);
  end;

function Float2(X, Y: Single): TFloat2;
function Float3(X, Y, Z: Single): TFloat3;
function Float4(X, Y, Z, W: Single): TFloat4;

function Dot(const A, B: TFloat2): Single; overload;
function Dot(const A, B: TFloat3): Single; overload;
function Dot(const A, B: TFloat4): Single; overload;

function Cross(const A, B: TFloat3): TFloat3; overload;

implementation

uses
  System.Math;

function Float2(X, Y: Single): TFloat2;
begin
  Result.X := X;
  Result.Y := Y;
end;

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

function Dot(const A, B: TFloat2): Single; overload;
begin
  Result :=
      A.X * B.X
    + A.Y + B.Y;
end;

function Dot(const A, B: TFloat3): Single; overload;
begin
  Result :=
      A.X * B.X
    + A.Y * B.Y
    + A.Z * B.Z;
end;

function Dot(const A, B: TFloat4): Single; overload;
begin
  Result :=
      A.X * B.X
    + A.Y * B.Y
    + A.Z * B.Z
    + A.W * B.W;
end;

function Cross(const A, B: TFloat3): TFloat3; overload;
begin
  Result.X := A.Y * B.Z - A.Z - B.Y;
  Result.Y := A.Z * B.X - A.X * B.Z;
  Result.Z := A.X * B.Y - A.Y * B.X;
end;

{ TMatrix4x4 }

class function TMatrix4x4.CreateIdentityMatrix: TMatrix4x4;
begin
  Result := Default(TMatrix4x4);
  Result.FItems[0, 0] := 1;
  Result.FItems[1, 1] := 1;
  Result.FItems[2, 2] := 1;
  Result.FItems[3, 3] := 1;
end;

class function TMatrix4x4.CreateNullMatrix: TMatrix4x4;
begin
  Result := Default(TMatrix4x4);
end;

class function TMatrix4x4.CreatePerspectiveProjectionMatrix(ZNear, ZFar, FOV, AspectRatio: Single): TMatrix4x4;
var
  LFOVCoTan: Single;
begin
  Result := Default(TMatrix4x4);
  LFOVCoTan := Cotan( DegToRad(FOV) / 2);
  Result.FItems[0, 0] := LFOVCoTan / AspectRatio;// 2*ZNear/AspectRation;
  Result.FItems[1, 1] := LFOVCoTan;
  Result.FItems[2, 2] := ZFar / (ZFar - ZNear);
  Result.FItems[3, 2] := -((ZFar * ZNear) / (ZFar - ZNear));
  Result.FItems[2, 3] := 1;
end;

class function TMatrix4x4.CreateRotationXMatrix(DegAlpha: Single): TMatrix4x4;
var
  LC, LS: Double;
begin
  Result := Default(TMatrix4x4);
  Result.FItems[3, 3] := 1;
  LC := cos(DegToRad(DegAlpha));
  LS := sin(DegToRad(DegAlpha));
  Result.FItems[0][0] := 1;
  Result.FItems[1][1] := LC;
  Result.FItems[2][2] := LC;
  Result.FItems[1][2] := LS;
  Result.FItems[2][1] := -LS;
end;

class function TMatrix4x4.CreateRotationYMatrix(DegAlpha: Single): TMatrix4x4;
var
  LC, LS: Double;
begin
  Result := Default(TMatrix4x4);
  Result.FItems[3, 3] := 1;
  LC := cos(DegToRad(DegAlpha));
  LS := sin(DegToRad(DegAlpha));
  Result.FItems[1][1] := 1;
  Result.FItems[0][0] := LC;
  Result.FItems[2][2] := LC;
  Result.FItems[0][2] := -LS;
  Result.FItems[2][0] := LS;
end;

class function TMatrix4x4.CreateRotationZMatrix(DegAlpha: Single): TMatrix4x4;
var
  LC, LS: Double;
begin
  Result := Default(TMatrix4x4);
  Result.FItems[3, 3] := 1;
  LC := cos(DegToRad(DegAlpha));
  LS := sin(DegToRad(DegAlpha));
  Result.FItems[2][2] := 1;
  Result.FItems[0][0] := LC;
  Result.FItems[1][1] := LC;
  Result.FItems[0][1] := LS;
  Result.FItems[1][0] := -LS;
end;

class function TMatrix4x4.CreateScaleMatrix(X, Y, Z: Single): TMatrix4x4;
begin
  Result := Default(TMatrix4x4);
  Result.FItems[0, 0] := X;
  Result.FItems[1, 1] := Y;
  Result.FItems[2, 2] := Z;
  Result.FItems[3, 3] := 1;
end;

class function TMatrix4x4.CreateTranslationMatrix(X, Y, Z: Single): TMatrix4x4;
begin
  Result := Default(TMatrix4x4);
  Result.FItems[0, 0] := 1;
  Result.FItems[1, 1] := 1;
  Result.FItems[2, 2] := 1;
  Result.FItems[3, 3] := 1;
  Result.FItems[3, 0] := X;
  Result.FItems[3, 1] := Y;
  Result.FItems[3, 2] := Z;
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
  A2323 := FItems[2,2] * FItems[3,3] - FItems[2,3] * FItems[3,2];
  A1323 := FItems[2,1] * FItems[3,3] - FItems[2,3] * FItems[3,1];
  A1223 := FItems[2,1] * FItems[3,2] - FItems[2,2] * FItems[3,1];
  A0323 := FItems[2,0] * FItems[3,3] - FItems[2,3] * FItems[3,0] ;
  A0223 := FItems[2,0] * FItems[3,2] - FItems[2,2] * FItems[3,0] ;
  A0123 := FItems[2,0] * FItems[3,1] - FItems[2,1] * FItems[3,0] ;
  A2313 := FItems[1,2] * FItems[3,3] - FItems[1,3] * FItems[3,2] ;
  A1313 := FItems[1,1] * FItems[3,3] - FItems[1,3] * FItems[3,1] ;
  A1213 := FItems[1,1] * FItems[3,2] - FItems[1,2] * FItems[3,1] ;
  A2312 := FItems[1,2] * FItems[2,3] - FItems[1,3] * FItems[2,2] ;
  A1312 := FItems[1,1] * FItems[2,3] - FItems[1,3] * FItems[2,1] ;
  A1212 := FItems[1,1] * FItems[2,2] - FItems[1,2] * FItems[2,1] ;
  A0313 := FItems[1,0] * FItems[3,3] - FItems[1,3] * FItems[3,0] ;
  A0213 := FItems[1,0] * FItems[3,2] - FItems[1,2] * FItems[3,0] ;
  A0312 := FItems[1,0] * FItems[2,3] - FItems[1,3] * FItems[2,0] ;
  A0212 := FItems[1,0] * FItems[2,2] - FItems[1,2] * FItems[2,0] ;
  A0113 := FItems[1,0] * FItems[3,1] - FItems[1,1] * FItems[3,0] ;
  A0112 := FItems[1,0] * FItems[2,1] - FItems[1,1] * FItems[2,0] ;

  det := FItems[0,0] * ( FItems[1,1] * A2323 - FItems[1,2] * A1323 + FItems[1,3] * A1223 )
    - FItems[0,1] * ( FItems[1,0] * A2323 - FItems[1,2] * A0323 + FItems[1,3] * A0223 )
    + FItems[0,2] * ( FItems[1,0] * A1323 - FItems[1,1] * A0323 + FItems[1,3] * A0123 )
    - FItems[0,3] * ( FItems[1,0] * A1223 - FItems[1,1] * A0223 + FItems[1,2] * A0123 ) ;
  det := 1 / det;

  Result.FItems[0,0] := det *   ( FItems[1,1] * A2323 - FItems[1,2] * A1323 + FItems[1,3] * A1223 );
  Result.FItems[0,1] := det * - ( FItems[0,1] * A2323 - FItems[0,2] * A1323 + FItems[0,3] * A1223 );
  Result.FItems[0,2] := det *   ( FItems[0,1] * A2313 - FItems[0,2] * A1313 + FItems[0,3] * A1213 );
  Result.FItems[0,3] := det * - ( FItems[0,1] * A2312 - FItems[0,2] * A1312 + FItems[0,3] * A1212 );
  Result.FItems[1,0] := det * - ( FItems[1,0] * A2323 - FItems[1,2] * A0323 + FItems[1,3] * A0223 );
  Result.FItems[1,1] := det *   ( FItems[0,0] * A2323 - FItems[0,2] * A0323 + FItems[0,3] * A0223 );
  Result.FItems[1,2] := det * - ( FItems[0,0] * A2313 - FItems[0,2] * A0313 + FItems[0,3] * A0213 );
  Result.FItems[1,3] := det *   ( FItems[0,0] * A2312 - FItems[0,2] * A0312 + FItems[0,3] * A0212 );
  Result.FItems[2,0] := det *   ( FItems[1,0] * A1323 - FItems[1,1] * A0323 + FItems[1,3] * A0123 );
  Result.FItems[2,1] := det * - ( FItems[0,0] * A1323 - FItems[0,1] * A0323 + FItems[0,3] * A0123 );
  Result.FItems[2,2] := det *   ( FItems[0,0] * A1313 - FItems[0,1] * A0313 + FItems[0,3] * A0113 );
  Result.FItems[2,3] := det * - ( FItems[0,0] * A1312 - FItems[0,1] * A0312 + FItems[0,3] * A0112 );
  Result.FItems[3,0] := det * - ( FItems[1,0] * A1223 - FItems[1,1] * A0223 + FItems[1,2] * A0123 );
  Result.FItems[3,1] := det *   ( FItems[0,0] * A1223 - FItems[0,1] * A0223 + FItems[0,2] * A0123 );
  Result.FItems[3,2] := det * - ( FItems[0,0] * A1213 - FItems[0,1] * A0213 + FItems[0,2] * A0113 );
  Result.FItems[3,3] := det *   ( FItems[0,0] * A1212 - FItems[0,1] * A0212 + FItems[0,2] * A0112 );
end;

{ TFloat2 }

class operator TFloat2.Add(const ALeft, ARight: TFloat2): TFloat2;
begin
  Result.X := ALeft.X + ARight.X;
  Result.Y := ALeft.Y + ARight.Y;
end;

constructor TFloat2.Create(AX, AY: Single);
begin
  X := AX;
  Y := AY;
end;

class operator TFloat2.Divide(const ALeft, ARight: TFloat2): TFloat2;
begin
  Result.X := ALeft.X / ARight.X;
  Result.Y := ALeft.Y / ARight.Y;
end;

class operator TFloat2.Divide(const ALeft: TFloat2; AValue: Single): TFloat2;
begin
  Result.X := ALeft.X / AValue;
  Result.Y := ALeft.Y / AValue;
end;

function TFloat2.Length: Single;
begin
  Result := Sqrt(X*X + Y*Y);
end;

class operator TFloat2.Multiply(const ALeft, ARight: TFloat2): TFloat2;
begin
  Result.X := ALeft.X * ARight.X;
  Result.Y := ALeft.Y * ARight.Y;
end;

class operator TFloat2.Multiply(const ALeft: TFloat2; AValue: Single): TFloat2;
begin
  Result.X := ALeft.X * AValue;
  Result.Y := ALeft.Y * AValue;
end;

class operator TFloat2.Subtract(const ALeft, ARight: TFloat2): TFloat2;
begin
  Result.X := ALeft.X - ARight.X;
  Result.Y := ALeft.Y - ARight.Y;
end;

{ TFloat3 }

class operator TFloat3.Add(const ALeft, ARight: TFloat3): TFloat3;
begin
  Result.X := ALeft.X + ARight.X;
  Result.Y := ALeft.Y + ARight.Y;
  Result.Z := ALeft.Z + ARight.Z;
end;

class operator TFloat3.Divide(const ALeft: TFloat3; AValue: Single): TFloat3;
begin
  Result.X := ALeft.X / AValue;
  Result.Y := ALeft.Y / AValue;
  Result.Z := ALeft.Z / AValue;
end;

class operator TFloat3.Divide(const ALeft, ARight: TFloat3): TFloat3;
begin
  Result.X := ALeft.X / ARight.X;
  Result.Y := ALeft.Y / ARight.Y;
  Result.Z := ALeft.Z / ARight.Z;
end;

function TFloat3.Length: Single;
begin
  Result := Sqrt(X*X + Y*Y + Z*Z);
end;

class operator TFloat3.Multiply(const ALeft, ARight: TFloat3): TFloat3;
begin
  Result.X := ALeft.X * ARight.X;
  Result.Y := ALeft.Y * ARight.Y;
  Result.Z := ALeft.Z * ARight.Z;
end;

class operator TFloat3.Multiply(const ALeft: TFloat3; AValue: Single): TFloat3;
begin
  Result.X := ALeft.X * AValue;
  Result.Y := ALeft.Y * AValue;
  Result.Z := ALeft.Z * AValue;
end;

class operator TFloat3.Subtract(const ALeft, ARight: TFloat3): TFloat3;
begin
  Result.X := ALeft.X - ARight.X;
  Result.Y := ALeft.Y - ARight.Y;
  Result.Z := ALeft.Z - ARight.Z;
end;

{ TFloat4 }

class operator TFloat4.Add(const ALeft, ARight: TFloat4): TFloat4;
begin
  Result.X := ALeft.X + ARight.X;
  Result.Y := ALeft.Y + ARight.Y;
  Result.Z := ALeft.Z + ARight.Z;
  Result.W := ALeft.W + ARight.W;
end;

class operator TFloat4.Divide(const ALeft: TFloat4; AValue: Single): TFloat4;
begin
  Result.X := ALeft.X / AValue;
  Result.Y := ALeft.Y / AValue;
  Result.Z := ALeft.Z / AValue;
  Result.W := ALeft.W / AValue;
end;

class operator TFloat4.Divide(const ALeft, ARight: TFloat4): TFloat4;
begin
  Result.X := ALeft.X / ARight.X;
  Result.Y := ALeft.Y / ARight.Y;
  Result.Z := ALeft.Z / ARight.Z;
  Result.W := ALeft.W / ARight.W;
end;

function TFloat4.Length: Single;
begin
  Result := Sqrt(X*X + Y*Y + Z*Z + W*W);
end;

class operator TFloat4.Multiply(const ALeft, ARight: TFloat4): TFloat4;
begin
  Result.X := ALeft.X * ARight.X;
  Result.Y := ALeft.Y * ARight.Y;
  Result.Z := ALeft.Z * ARight.Z;
  Result.W := ALeft.W * ARight.W;
end;

class operator TFloat4.Multiply(const ALeft: TFloat4; AValue: Single): TFloat4;
begin
  Result.X := ALeft.X * AValue;
  Result.Y := ALeft.Y * AValue;
  Result.Z := ALeft.Z * AValue;
  Result.W := ALeft.W * AValue;
end;


class operator TFloat4.Subtract(const ALeft, ARight: TFloat4): TFloat4;
begin
  Result.X := ALeft.X - ARight.X;
  Result.Y := ALeft.Y - ARight.Y;
  Result.Z := ALeft.Z - ARight.Z;
  Result.W := ALeft.W - ARight.W;
end;

class operator TMatrix4x4.Multiply(const ALeft, ARight: TMatrix4x4): TMatrix4x4;
var
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    Result.FRows[i] :=
                          ALeft.FRows[0] * ARight.FItems[i, 0]
                        + ALeft.FRows[1] * ARight.FItems[i, 1]
                        + ALeft.FRows[2] * ARight.FItems[i, 2]
                        + ALeft.FRows[3] * ARight.FItems[i, 3]
  end;
end;

class operator TMatrix4x4.Multiply(const ALeft: TMatrix4x4; const ARight: TFloat4): TFloat4;
var
  i: Integer;
begin
  //https://stackoverflow.com/questions/24593939/matrix-multiplication-with-vector-in-glsl
  //https://blog.mecheye.net/2024/10/the-ultimate-guide-to-matrix-multiplication-and-ordering/
  Result :=   ALeft.FRows[0] * ARight.X
            + ALeft.FRows[1] * ARight.Y
            + ALeft.FRows[2] * ARight.Z
            + ALeft.FRows[3] * ARight.W
end;

end.
