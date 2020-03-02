unit Rasterizer;

interface

uses
  Math3D,
  Shader;

procedure RasterizeTriangle(AMaxResolutionX, AMaxResolutionY: Integer; AVerctorA, AvectorB, AvectorC: TVectorClass4D; AShader: TShader; ABlockOffset, ABlockStep: Integer);

implementation

uses
  Types,
  Math;

const
  QuadSize = 8;

procedure RasterizeTriangle(AMaxResolutionX, AMaxResolutionY: Integer; AVerctorA, AvectorB, AvectorC: TVectorClass4D; AShader: TShader; ABlockOffset, ABlockStep: Integer);
var
  Y1, Y2, Y3, X1, X2, X3, DX12, DX23, DX31, DY12, DY23, DY31, FDX12, FDX23, FDX31, FDY12, FDY23, FDY31: Integer;
  MinX, MinY, MAxX, MAxY, C1, C2, C3, BlockX, BlockY, CornerX0, CornerX1, CornerY0, CornerY1: Integer;
  CY1, CY2, CY3, CX1, CX2, CX3: Integer;
  A00, A10, A01, A11, B00, B10, B01, B11, C00, C10, C01, C11, ResultOrA, ResultAndA, ResultOrB, ResultAndB,
  ResultOrC, ResultAndC: Boolean;
  i, k: Integer;
begin
// 28.4 fixed-point coordinates
    X1 := Round(16*AVerctorA.Element[0]);
    X2 := Round(16*AVectorB.Element[0]);
    X3 := Round(16*AvectorC.Element[0]);

    Y1 := Round(16*AVerctorA.Element[1]);
    Y2 := Round(16*AVectorB.Element[1]);
    Y3 := Round(16*AvectorC.Element[1]);

//    Z1 := Round(AVerctorA.Element[2]);
//    Z2 := Round(AVectorB.Element[2]);
//    Z3 := Round(AvectorC.Element[2]);

    // Deltas
    DX12 := X1 - X2;
    DX23 := X2 - X3;
    DX31 := X3 - X1;

    DY12 := Y1 - Y2;
    DY23 := Y2 - Y3;
    DY31 := Y3 - Y1;

    // Fixed-point deltas
    FDX12 := DX12*16;// shl 4;
    FDX23 := DX23*16;// shl 4;
    FDX31 := DX31*16;// shl 4;

    FDY12 := DY12*16;// shl 4;
    FDY23 := DY23*16;// shl 4;
    FDY31 := DY31*16;// shl 4;

    // Bounding rectangle
//    minx := (min(X1, min(X2, X3)) + 15);// shr 4;
//    maxx := (max(X1, Max(X2, X3)) + 15);// shr 4;
//    miny := (min(Y1, Max(Y2, Y3)) + 15);// shr 4;
//    maxy := (max(Y1, MAx(Y2, Y3)) + 15);// shr 4;
    minx := Max(0, (min(X1, min(X2, X3)) + 15) div 16);// shr 4;
    maxx := Min(AMaxResolutionX, (max(X1, Max(X2, X3)) + 15) div 16);// shr 4;
    miny := Max(0, (min(Y1, min(Y2, Y3)) + 15) div 16);// shr 4;
    maxy := Min(AMaxResolutionY, (max(Y1, MAx(Y2, Y3)) + 15) div 16);// shr 4;
    AShader.MinX := minx;
    AShader.MinY := miny;


    // Start in corner of 8x8 block
//    minx &= ~(q - 1);
//    miny &= ~(q - 1);
    MinX := MinX div (QuadSize) * QuadSize;
    MinY := MinY div (QuadSize) * QuadSize;
        //align to block matching stepping
    MinY := MinY div (QuadSize*ABlockStep) * QuadSize*ABlockStep + ABlockOffset*QuadSize;

    // Half-edge constants
    C1 := DY12 * X1 - DX12 * Y1;
    C2 := DY23 * X2 - DX23 * Y2;
    C3 := DY31 * X3 - DX31 * Y3;

    // Correct for fill convention
    if(DY12 < 0) or ((DY12 = 0) and (DX12 > 0))then
    begin
      C1 := C1 + 1;
    end;
    if(DY23 < 0) or ((DY23 = 0) and (DX23 > 0))then
    begin
      C2 := C2 + 1;
    end;
    if(DY31 < 0) or ((DY31 = 0) and (DX31 > 0))then
    begin
      C3 := C3 + 1;
    end;

    // Loop through blocks
    BlockY := MinY;
    while BlockY < MaxY do
    begin
      BlockX := MinX;
        while BlockX < MaxX do
        begin
            // Corners of block
            CornerX0 := BlockX*16;// shl 4;
            CornerX1 := (BlockX + QuadSize - 1)*16;// shl 4;
            CornerY0 := BlockY*16;// shl 4;
            CornerY1 := (BlockY + QuadSize - 1)*16;// shl 4;

            // Evaluate half-space functions
            a00 := (C1 + DX12 * CornerY0 - DY12 * CornerX0) > 0;
            a10 := (C1 + DX12 * CornerY0 - DY12 * CornerX1) > 0;
            a01 := (C1 + DX12 * CornerY1 - DY12 * CornerX0) > 0;
            a11 := (C1 + DX12 * CornerY1 - DY12 * CornerX1) > 0;
            ResultOrA := a00 or a10 or a01 or a11;
            ResultAndA := a00 and a10 and a01 and a11;

            b00 := (C2 + DX23 * CornerY0 - DY23 * CornerX0) > 0;
            b10 := (C2 + DX23 * CornerY0 - DY23 * CornerX1) > 0;
            b01 := (C2 + DX23 * CornerY1 - DY23 * CornerX0) > 0;
            b11 := (C2 + DX23 * CornerY1 - DY23 * CornerX1) > 0;
            ResultOrB := B00 or B10 or B01 or B11;
            ResultAndB := B00 and B10 and B01 and B11;

            c00 := (C3 + DX31 * CornerY0 - DY31 * CornerX0) > 0;
            c10 := (C3 + DX31 * CornerY0 - DY31 * CornerX1) > 0;
            c01 := (C3 + DX31 * CornerY1 - DY31 * CornerX0) > 0;
            c11 := (C3 + DX31 * CornerY1 - DY31 * CornerX1) > 0;
            ResultOrC := C00 or C10 or C01 or C11;
            ResultAndC := C00 and C10 and C01 and C11;

            // Skip block when outside an edge
            if ResultOrA or ResultOrB or ResultOrC then
            begin
            // Accept whole block when totally covered
              if (ResultAndA and ResultAndB and ResultAndC)  then
              begin
                AShader.Pixel := Point(BlockX, BlockY);
                AShader.Shade8X8Quad();
              end
              else //Partially covered Block
              begin

                CY1 := C1 + DX12 * CornerY0 - DY12 * CornerX0;
                CY2 := C2 + DX23 * CornerY0 - DY23 * CornerX0;
                CY3 := C3 + DX31 * CornerY0 - DY31 * CornerX0;

                for i := BlockY to Min(AMaxResolutionY, BlockY + QuadSize - 1) do
                begin
                  CX1 := CY1;
                  CX2 := CY2;
                  CX3 := CY3;

                  for k := BlockX to Min(AMaxResolutionX, BlockX + QuadSize - 1) do
                  begin
                    if(CX1 >= 0) and (CX2 >= 0) and (CX3 >= 0)then
                    begin
                      AShader.Pixel := Point(k, i);
                      AShader.ShadeSinglePixel();
                    end;

                    CX1 := CX1 - FDY12;
                    CX2 := CX2 - FDY23;
                    CX3 := CX3 - FDY31;
                  end;

                  CY1 := CY1 + FDX12;
                  CY2 := CY2 + FDX23;
                  CY3 := CY3 + FDX31;
                end;
              end;
            end;
          BlockX := BlockX + QuadSize;
        end;
      BlockY := BlockY + QuadSize * ABlockStep;
    end;
end;

end.
