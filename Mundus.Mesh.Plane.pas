unit Mundus.Mesh.Plane;

interface

uses
  Mundus.Mesh,
  Mundus.Types,
  Mundus.Math;

type
  TPlane = class(TMesh)
  public
    constructor Create(AWidth, AHeight, ATesselation: Integer);
  end;

implementation

{ TPlane }

constructor TPlane.Create(AWidth, AHeight, ATesselation: Integer);
var
  LCellWidth, LCellHeight, LX, LY, LVerticesPerLine, LTotalVertices: Integer;
  i, k, LUV: Integer;
  LVector: TVector;
  LVertexA, LVertexB, LVertexC: Integer;
begin
  inherited Create();
  LCellWidth := AWidth div ATesselation;
  LCellHeight := AHeight div ATesselation;
  LVerticesPerLine := ATesselation + 1;
  LTotalVertices := LVerticesPerLine * LVerticesPerLine;
  SetLength(FUV, LTotalVertices);
  LY := -(AHeight div 2);
  LUV := 0;
  for i := 0 to ATesselation do
  begin
    LX := -(AWidth div 2);
    for k := 0 to ATesselation do
    begin
      LVector.X := LX;
      LVector.Y := LY;
      LVector.Z := 0;
      AddVertice(LVector);
      FUV[LUV] := TFloat2.Create(k mod 2, i mod 2);
      Inc(LX, LCellWidth);
      Inc(LUV);
    end;
    Inc(LY, LCellHeight);
  end;

  for i := 1 to ATesselation do
  begin
    for k := 1 to ATesselation do
    begin
      LVertexA := (k-1) + LVerticesPerLine * (i-1);
      LVertexB := LVertexA + 1;
      LVertexC := LVertexA + LVerticesPerLine;
      AddTriangle(Triangle(LVertexA, LVertexB, LVertexC));
      LVertexA := LVertexC + 1;
      AddTriangle(Triangle(LVertexC, LVertexB, LVertexA));
    end;
  end;
end;

end.
