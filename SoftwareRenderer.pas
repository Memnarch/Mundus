unit SoftwareRenderer;

interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Collections, Math3D, BaseMesh, ColorTypes, Shader, SiAuto, SmartInspect;

type
  TDepthBuffer = array of array of Single;

  TSoftwareRenderer = class
  private
    FDepthBuffer: TDepthBuffer;
    FBackBuffer: TBitmap;
    FTexture: TBitmap;
    FMeshList: TObjectList<TBaseMesh>;
    FFPS: Integer;
    FPolyCount: Cardinal;
    FLineLength: LongInt;
    FFirstLine: PRGB32Array;
    FQuadSize: Integer;
    procedure SetDepthBufferSize(AWidth, AHeight: Integer);
    procedure ClearDepthBuffer();
    procedure RasterizeTriangle(AVerctorA, AvectorB, AvectorC: TVectorClass4D; AShader: TShader);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure SetResolution(AWidth, AHeight: Integer);
    procedure RenderFrame(ACanvas: TCanvas);
    function GetCurrentFPS(): Integer;
    function GetCurrentPolyCount(): Cardinal;
    property MeshList: TObjectList<TBaseMesh> read FMeshList;
    property QuadSize: Integer read FQuadSize;
  end;

  function RGB32(ARed, AGreen, ABlue, AAlpha: Byte): TRGB32;

implementation
uses
  Math, DateUtils, SolidColorShader, DepthColorShader, TextureShader;

var
  GTest: Single  = 0;
  GTest2: Single = 0;

{ TSoftwareRenderer }

procedure TSoftwareRenderer.ClearDepthBuffer;
var
  i, k: Integer;
begin
  for i := 0 to High(FDepthBuffer) do
  begin
    for k := 0 to High(FDepthBuffer[i]) do
    begin
      FDepthBuffer[i, k] := 10000;
    end;
  end;
end;

constructor TSoftwareRenderer.Create;
begin
  FBackBuffer := TBitmap.Create();
  FBackbuffer.PixelFormat := pf32bit;
  SetResolution(512, 512);
  FMeshList := TObjectList<TBaseMesh>.Create();
  FQuadSize := 8;

  FTexture := TBitmap.Create();
  FTexture.LoadFromFile('Crate.bmp');
  FTexture.PixelFormat := pf32bit;
end;

destructor TSoftwareRenderer.Destroy;
begin
  FMeshList.Free();
  FBackBuffer.Free();
  FTexture.Free;
  inherited;
end;

function TSoftwareRenderer.GetCurrentFPS: Integer;
begin
  Result := FFPS;
end;

function TSoftwareRenderer.GetCurrentPolyCount: Cardinal;
begin
  Result := FPolyCount;
end;

procedure TSoftwareRenderer.RasterizeTriangle(AVerctorA, AvectorB,
  AvectorC: TVectorClass4D; AShader: TShader);
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
    minx := Max(0, (min(X1, min(X2, X3))) div 16);// shr 4;
    maxx := Min(511, max(X1, Max(X2, X3)) div 16);// shr 4;
    miny := Max(0, min(Y1, min(Y2, Y3)) div 16);// shr 4;
    maxy := Min(511, max(Y1, MAx(Y2, Y3)) div 16);// shr 4;
    AShader.MinX := minx;
    AShader.MinY := miny;


    // Start in corner of 8x8 block
//    minx &= ~(q - 1);
//    miny &= ~(q - 1);
    MinX := MinX div (QuadSize) * QuadSize;
    MinY := MinY div (QuadSize) * QuadSize;

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

                for i := BlockY to Min(511, BlockY + QuadSize - 1) do
                begin
                  CX1 := CY1;
                  CX2 := CY2;
                  CX3 := CY3;

                  for k := BlockX to Min(511, BlockX + QuadSize - 1) do
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
      BlockY := BlockY + QuadSize;
    end;
end;

procedure TSoftwareRenderer.RenderFrame(ACanvas: TCanvas);
var
  LTimeStart: TDateTime;
  LMesh: TBaseMesh;
  LTriangle: TTriangleClass;
  LWorldMatrix, LViewMatrix, LProjectionMatrix: TMatrixClass4D;  // Gerade dabei gewesen Viewtransformation einzubauen!! Seite 86 in TeilB
  LMoveMatrix: TMatrixClass4D;
  LRotateXMatrix, LRotateYMatrix, LRotateZMatrix: TMatrixClass4D;
  LVertexA, LVertexB, LVertexC, LNormal: TVectorClass4D;
  LShader: TTextureShader;
begin
  LShader := TTextureShader.Create(FBackBuffer); //TSolidColorShader.Create(FBackBuffer);
  LTimeStart := Now();
  FBackBuffer.Canvas.FillRect(FBackBuffer.Canvas.ClipRect);
  ClearDepthBuffer();
  LWorldMatrix := TMatrixClass4D.Create();
  LProjectionMatrix := TMatrixClass4D.Create();
  LViewMatrix := TMatrixClass4D.Create();
  LMoveMatrix := TMatrixClass4D.Create();
  LRotateXMatrix := TMatrixClass4D.Create();
  LRotateYMatrix := TMatrixClass4D.Create();
  LRotateZMatrix := TMatrixClass4D.Create();

  LVertexA := TVectorClass4D.Create();
  LVertexB := TVectorClass4D.Create();
  LVertexC := TVectorClass4D.Create();
  LNormal := TVectorClass4D.Create();

  LViewMatrix.SetAsMoveMatrix(0, 0, 0);
  LRotateXMatrix.SetAsRotationXMatrix(DegToRad(0));
  LRotateYMatrix.SetAsRotationYMatrix(DegToRad(0));
  LRotateZMatrix.SetAsRotationZMatrix(DegToRad(0));
  LViewMatrix.MultiplyMatrix4D(LRotateXMatrix);
  LViewMatrix.MultiplyMatrix4D(LRotateYMatrix);
  LViewMatrix.MultiplyMatrix4D(LRotateZMatrix);


  FPolyCount := 0;
  for LMesh in FMeshList do
  begin
    LMoveMatrix.SetAsMoveMatrix(LMesh.Position.X, LMesh.Position.Y, LMesh.Position.Z);
    LRotateXMatrix.SetAsRotationXMatrix(DegToRad(GTest));
    LRotateYMatrix.SetAsRotationYMatrix(DegToRad(GTest2));
    LRotateZMatrix.SetAsRotationZMatrix(DegToRad(0));
    LWorldMatrix.CopyFromMatrix4D(LMoveMatrix);
    LWorldMatrix.MultiplyMatrix4D(LRotateXMatrix);
    LWorldMatrix.MultiplyMatrix4D(LRotateYMatrix);
    LWorldMatrix.MultiplyMatrix4D(LRotateZMatrix);
    LWorldMatrix.MultiplyMatrix4D(LViewMatrix);
    LProjectionMatrix.SetAsPerspectiveProjectionMatrix(100, 200, 64, 64);
//    LProjectionMatrix.SetAsPerspectiveProjectionMatrix(100, 200, 12000, 12000);
    LProjectionMatrix.MultiplyMatrix4D(LWorldMatrix);
    for LTriangle in LMesh.Triangles do
    begin
      LVertexA.Element[0] := LMesh.Vertices.Items[LTriangle.VertexA].X;
      LVertexA.Element[1] := LMesh.Vertices.Items[LTriangle.VertexA].Y;
      LVertexA.Element[2] := LMesh.Vertices.Items[LTriangle.VertexA].Z;
      LVertexA.Element[3] := 1;
      LVertexA.MultiplyWithMatrix4D(LProjectionMatrix);
      LVertexA.Rescale(True);




      LVertexB.Element[0] := LMesh.Vertices.Items[LTriangle.VertexB].X;
      LVertexB.Element[1] := LMesh.Vertices.Items[LTriangle.VertexB].Y;
      LVertexB.Element[2] := LMesh.Vertices.Items[LTriangle.VertexB].Z;
      LVertexB.Element[3] := 1;
      LVertexB.MultiplyWithMatrix4D(LProjectionMatrix);
      LVertexB.Rescale(True);




      LVertexC.Element[0] := LMesh.Vertices.Items[LTriangle.VertexC].X;
      LVertexC.Element[1] := LMesh.Vertices.Items[LTriangle.VertexC].Y;
      LVertexC.Element[2] := LMesh.Vertices.Items[LTriangle.VertexC].Z;
      LVertexC.Element[3] := 1;
      LVertexC.MultiplyWithMatrix4D(LProjectionMatrix);
      LVertexC.Rescale(True);


      LNormal.CalculateSurfaceNormal(LVertexA, LVertexB, LVertexC);
      if LNormal.Z < 0 then
      begin
        //denormalize vectors to screenpos
        LVertexA.Element[0] := (1-LVertexA.Element[0]) * 256;//half screen size
        LVertexA.Element[1] := (1-LVertexA.Element[1]) * 256;
        LVertexB.Element[0] := (1-LVertexB.Element[0]) * 256;
        LVertexB.Element[1] := (1-LVertexB.Element[1]) * 256;
        LVertexC.Element[0] := (1-LVertexC.Element[0]) * 256;
        LVertexC.Element[1] := (1-LVertexC.Element[1]) * 256;
//        LVertexA.MultiplyVector4D(256);
//        LVertexB.MultiplyVector4D(256);
//        LVertexC.MultiplyVector4D(256);
        LShader.InitTriangle(LVertexA, LVertexB, LVertexC);
        LShader.InitUV(LTriangle.UVA, LTriangle.UVB, LTriangle.UVC);
        LShader.InitTexture(FTexture);
        RasterizeTriangle(LVertexA, LVertexB,
          LVertexC, LShader);

//        FBackBuffer.Canvas.Pen.Color := clWhite;
//        FBackBuffer.Canvas.PolyLine([
//          Point(Round(LVertexA.Element[0]), Round(LVertexA.Element[1])),
//          Point(Round(LVertexB.Element[0]), Round(LVertexB.Element[1])),
//          Point(Round(LVertexC.Element[0]), Round(LVertexC.Element[1])),
//          Point(Round(LVertexA.Element[0]), Round(LVertexA.Element[1]))
//          ]);
//        FBackBuffer.Canvas.Brush.Color := clYellow;
//        FBackBuffer.Canvas.Pen.Color := clYellow;
//        FBackBuffer.Canvas.Rectangle(Round(LVertexA.Element[0])-2, Round(LVertexA.Element[1])-2, Round(LVertexA.Element[0])+2, Round(LVertexA.Element[1])+2);
//        FBackBuffer.Canvas.Rectangle(Round(LVertexB.Element[0])-2, Round(LVertexB.Element[1])-2, Round(LVertexB.Element[0])+2, Round(LVertexB.Element[1])+2);
//        FBackBuffer.Canvas.Rectangle(Round(LVertexC.Element[0])-2, Round(LVertexC.Element[1])-2, Round(LVertexC.Element[0])+2, Round(LVertexC.Element[1])+2);
//        FBackBuffer.Canvas.Brush.Color := clBlack;
//        FBackBuffer.Canvas.Pen.Color := clBlack;
        FPolyCount := FPolyCount + 1;
      end;
    end;
  end;
//  RasterizeTriangle(Vector(0, 0, 0), Vector(25, 0, 0),
//        Vector(25, 25,0), RGB32(255, 0, 0, 0), RGB32(255, 0, 0, 0), RGB32(255, 0, 0, 0));
  ACanvas.Draw(0, 0, FBackBuffer);
  FFPS := 1000 div Max(1, MilliSecondsBetween(Now(), LTimeStart));
  LWorldMatrix.Free();
  LMoveMatrix.Free();
  LProjectionMatrix.Free();
  LRotateXMAtrix.Free();
  LRotateYMatrix.Free();
  LRotateZMatrix.Free();
  LViewMatrix.Free();
  LVertexA.Free();
  LVertexB.Free();
  LVertexC.Free();
  LNormal.Free;
  GTest := GTest + 0.25;
  GTest2 := 45;//GTest2 + 0.25;

  LShader.Free();
end;

procedure TSoftwareRenderer.SetDepthBufferSize(AWidth, AHeight: Integer);
var
  i: Integer;
begin
  SetLength(FDepthBuffer, AHeight);
  for i := 0 to AHeight - 1 do
  begin
    SetLength(FDepthBuffer[i], AWidth);
  end;
end;

procedure TSoftwareRenderer.SetResolution(AWidth, AHeight: Integer);
begin
  FBackBuffer.SetSize(AWidth, Aheight);
  FFirstLIne := FBackBuffer.ScanLine[0];
  FLineLength := (Longint(FBackBuffer.Scanline[1]) - Longint(FFirstLine)) div SizeOf(TRGB32);
  FBackBuffer.Canvas.Pen.Color := clBlack;
  FBackBuffer.Canvas.Brush.Color := clBlack;
  SetDepthBufferSize(AWidth, AHeight);
end;

{ some functions }

function RGB32(ARed, AGreen, ABlue, AAlpha: Byte): TRGB32;
begin
  Result.R := ARed;
  Result.G := AGreen;
  Result.B := ABlue;
  Result.A := AAlpha;
end;

end.
