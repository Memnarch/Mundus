unit SoftwareRenderer;

interface

uses
  Types, Classes, Windows, SysUtils, Graphics,
  Generics.Collections, Math3D, BaseMesh,
  ColorTypes, Shader, StopWatch;

type
  TDepthBuffer = array of array of Single;
  TRenderEvent = procedure(Canvas: TCanvas) of object;

  TSoftwareRenderer = class
  private
    FDepthBuffer: TDepthBuffer;
    FZeroDepthBuffer: TDepthBuffer;
    FBackBuffer: TBitmap;
    FTexture: TBitmap;
    FMeshList: TObjectList<TBaseMesh>;
    FFPS: Integer;
    FPolyCount: Cardinal;
    FLineLength: LongInt;
    FFirstLine: PRGB32Array;
    FQuadSize: Integer;
    FResolutionX: Integer;
    FResolutionY: Integer;
    FMaxResolutionX: Integer;
    FMaxResolutionY: Integer;
    FHalfResolutionX: Integer;
    FHalfResolutionY: Integer;
    FOnAfterFrame: TRenderEvent;
    FTimer: TStopWatch;
    //buffers for rendering the frame
    FWorldMatrix: TMatrixClass4D;
    FViewMatrix: TMatrixClass4D;
    FProjectionMatrix: TMatrixClass4D;  // Gerade dabei gewesen Viewtransformation einzubauen!! Seite 86 in TeilB
    FMoveMatrix: TMatrixClass4D;
    FRotateXMatrix: TMatrixClass4D;
    FRotateYMatrix: TMatrixClass4D;
    FRotateZMatrix: TMatrixClass4D;
    FVertexA: TVectorClass4D;
    FVertexB: TVectorClass4D;
    FVertexC: TVectorClass4D;
    FNormal: TVectorClass4D;
    FShader: TShader;
    procedure SetDepthBufferSize(AWidth, AHeight: Integer);
    procedure ClearDepthBuffer();
    procedure RasterizeTriangle(AVerctorA, AvectorB, AvectorC: TVectorClass4D; AShader: TShader);
    procedure TransformMesh(AMesh: TBaseMesh; AMatrix: TMatrixClass4D);
    procedure DoAfterFrame(ACanvas: TCanvas);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure SetResolution(AWidth, AHeight: Integer);
    procedure RenderFrame(ACanvas: TCanvas);
    function GetCurrentFPS(): Integer;
    function GetCurrentPolyCount(): Cardinal;
    property MeshList: TObjectList<TBaseMesh> read FMeshList;
    property QuadSize: Integer read FQuadSize;
    property OnAfterFrame: TRenderEvent read FOnAfterFrame write FOnAfterFrame;
    property ResolutionX: Integer read FResolutionX;
    property ResolutionY: Integer read FResolutionY;
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
begin
  FDepthBuffer := FZeroDepthBuffer;
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
  FTimer := TStopWatch.Create(False);

  //setting up objects used while rendering a frame
  FShader := TTextureShader.Create(); //TSolidColorShader.Create(FBackBuffer);
  FWorldMatrix := TMatrixClass4D.Create();
  FProjectionMatrix := TMatrixClass4D.Create();
  FViewMatrix := TMatrixClass4D.Create();
  FMoveMatrix := TMatrixClass4D.Create();
  FRotateXMatrix := TMatrixClass4D.Create();
  FRotateYMatrix := TMatrixClass4D.Create();
  FRotateZMatrix := TMatrixClass4D.Create();

  FVertexA := TVectorClass4D.Create();
  FVertexB := TVectorClass4D.Create();
  FVertexC := TVectorClass4D.Create();
  FNormal := TVectorClass4D.Create();
end;

destructor TSoftwareRenderer.Destroy;
begin
  FMeshList.Free();
  FBackBuffer.Free();
  FTexture.Free;
  FTimer.Free;
  //destroy objects used while rendering a frame
  FShader.Free();
  FWorldMatrix.Free();
  FMoveMatrix.Free();
  FProjectionMatrix.Free();
  FRotateXMAtrix.Free();
  FRotateYMatrix.Free();
  FRotateZMatrix.Free();
  FViewMatrix.Free();
  FVertexA.Free;
  FVertexB.Free;
  FVertexC.Free;
  FNormal.Free;
  inherited;
end;

procedure TSoftwareRenderer.DoAfterFrame(ACanvas: TCanvas);
begin
  if Assigned(FOnAfterFrame) then
  begin
    FOnAfterFrame(ACanvas);
  end;
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
    minx := Max(0, (min(X1, min(X2, X3)) + 15) div 16);// shr 4;
    maxx := Min(FMaxResolutionX, (max(X1, Max(X2, X3)) + 15) div 16);// shr 4;
    miny := Max(0, (min(Y1, min(Y2, Y3)) + 15) div 16);// shr 4;
    maxy := Min(FMaxResolutionY, (max(Y1, MAx(Y2, Y3)) + 15) div 16);// shr 4;
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

                for i := BlockY to Min(FMaxResolutionY, BlockY + QuadSize - 1) do
                begin
                  CX1 := CY1;
                  CX2 := CY2;
                  CX3 := CY3;

                  for k := BlockX to Min(FMaxResolutionX, BlockX + QuadSize - 1) do
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
  LMesh: TBaseMesh;
  LTriangle: TTriangleClass;
begin
  FTimer.Start();
  FShader.PixelBuffer := FBackBuffer;
  FBackBuffer.Canvas.Brush.Color := clRed;
  FBackBuffer.Canvas.FillRect(FBackBuffer.Canvas.ClipRect);
  ClearDepthBuffer();

  FViewMatrix.SetAsMoveMatrix(0, 0, 0);
  FRotateXMatrix.SetAsRotationXMatrix(DegToRad(0));
  FRotateYMatrix.SetAsRotationYMatrix(DegToRad(0));
  FRotateZMatrix.SetAsRotationZMatrix(DegToRad(0));
  FViewMatrix.MultiplyMatrix4D(FRotateXMatrix);
  FViewMatrix.MultiplyMatrix4D(FRotateYMatrix);
  FViewMatrix.MultiplyMatrix4D(FRotateZMatrix);


  FPolyCount := 0;
  for LMesh in FMeshList do
  begin
    FMoveMatrix.SetAsMoveMatrix(LMesh.Position.X, LMesh.Position.Y, LMesh.Position.Z);
    FRotateXMatrix.SetAsRotationXMatrix(DegToRad(GTest));
    FRotateYMatrix.SetAsRotationYMatrix(DegToRad(GTest2));
    FRotateZMatrix.SetAsRotationZMatrix(DegToRad(0));
    FWorldMatrix.CopyFromMatrix4D(FMoveMatrix);
    FWorldMatrix.MultiplyMatrix4D(FRotateXMatrix);
    FWorldMatrix.MultiplyMatrix4D(FRotateYMatrix);
    FWorldMatrix.MultiplyMatrix4D(FRotateZMatrix);
    FWorldMatrix.MultiplyMatrix4D(FViewMatrix);
    FProjectionMatrix.SetAsPerspectiveProjectionMatrix(100, 200, 0.7, FResolutionX/FResolutionY);
    FProjectionMatrix.MultiplyMatrix4D(FWorldMatrix);
    TransformMesh(LMesh, FProjectionMatrix);
    for LTriangle in LMesh.Triangles do
    begin
      FVertexA.CopyFromVector4D(LMesh.TransformedVertices[LTriangle.VertexA]);
      FVertexB.CopyFromVector4D(LMesh.TransformedVertices[LTriangle.VertexB]);
      FVertexC.CopyFromVector4D(LMesh.TransformedVertices[LTriangle.VertexC]);


      FNormal.CalculateSurfaceNormal(FVertexA, FVertexB, FVertexC);
      if (FNormal.Z < 0)then
      begin
        //denormalize vectors to screenpos
        FVertexA.Element[0] := (1-FVertexA.Element[0]) * FHalfResolutionX;//half screen size
        FVertexA.Element[1] := (1-FVertexA.Element[1]) * FHalfResolutionY;
        FVertexB.Element[0] := (1-FVertexB.Element[0]) * FHalfResolutionX;
        FVertexB.Element[1] := (1-FVertexB.Element[1]) * FHalfResolutionY;
        FVertexC.Element[0] := (1-FVertexC.Element[0]) * FHalfResolutionX;
        FVertexC.Element[1] := (1-FVertexC.Element[1]) * FHalfResolutionY;

        FShader.InitTriangle(FVertexA, FVertexB, FVertexC);
        TTextureShader(FShader).InitUV(LTriangle.UVA, LTriangle.UVB, LTriangle.UVC);
        TTextureShader(FShader).InitTexture(FTexture);
        RasterizeTriangle(FVertexA, FVertexB,
          FVertexC, FShader);

        FPolyCount := FPolyCount + 1;
      end;
    end;
  end;

  DoAfterFrame(FBackBuffer.Canvas);
  ACanvas.Draw(0, 0, FBackBuffer);
  FTimer.Stop();
  FFPS := 1000000 div FTimer.ElapsedMicroseconds;

  GTest := GTest + 0.25;
  GTest2 := 45;
end;

procedure TSoftwareRenderer.SetDepthBufferSize(AWidth, AHeight: Integer);
var
  i: Integer;
begin
  SetLength(FDepthBuffer, AHeight);
  SetLength(FZeroDepthBuffer, AHeight);
  for i := 0 to AHeight - 1 do
  begin
    SetLength(FDepthBuffer[i], AWidth);
    SetLength(FZeroDepthBuffer[i], AWidth);
    ZeroMemory(FZeroDepthBuffer[i], AWidth);
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
  ClearDepthBuffer();
  FResolutionX := AWidth;
  FResolutionY := AHeight;
  FMaxResolutionX := FResolutionX - 1;
  FMaxResolutionY := FResolutionY - 1;
  FHalfResolutionX := FResolutionX div 2;
  FHalfResolutionY := FResolutionY div 2;
end;

procedure TSoftwareRenderer.TransformMesh(AMesh: TBaseMesh;
  AMatrix: TMatrixClass4D);
var
  i: Integer;
begin
  for i := 0 to AMesh.Vertices.Count - 1 do
  begin
    AMesh.TransformedVertices[i].Element[0] := AMesh.Vertices[i].X;
    AMesh.TransformedVertices[i].Element[1] := AMesh.Vertices[i].Y;
    AMesh.TransformedVertices[i].Element[2] := AMesh.Vertices[i].Z;
    AMesh.TransformedVertices[i].Element[3] := 1;
    AMesh.TransformedVertices[i].MultiplyWithMatrix4D(AMatrix);
    AMesh.TransformedVertices[i].Rescale(True);
  end;
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
