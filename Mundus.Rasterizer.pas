unit Mundus.Rasterizer;

interface

uses
  Math,
  Mundus.Math,
  Mundus.Shader,
  Mundus.Types,
  Mundus.Math.Interpolation;

  {$IFDEF Debug}
    {$Inline Off}
  {$ENDIF}

type
  TDepthTest = class
  end;

  TNoDepth = class(TDepthTest);
  TDepthTestOnly = class(TDepthTest);
  TDepthWrite = class(TDepthTest);

  THalfEdgeConstants = packed record
    C1, C2, C3: Integer;
  end;

  PHalfEdgeConstants = ^THalfEdgeConstants;

  THalfSpaceDeltas = packed record
    X12, X23, X31: Integer;
    Y12, Y23, Y31: Integer;
  end;

  PHalfSpaceDeltas = ^THalfSpaceDeltas;

  TBlockCorners = packed record
    X0, X1: Integer;
    Y0, Y1: Integer;
  end;

  PBlockCorners = ^TBlockCorners;

  TBlockState = packed record
    Intersects: Boolean;
    IsFullBlock: Boolean;
  end;

  PBlockState = ^TBlockState;

  TTrianglePosition = packed record
    _1, _2, _3: Integer;
  end;

  PTrianglePosition = ^TTrianglePosition;

  TRasterizerFactory<TAttributes: record; Shader: TShader<TAttributes>; DepthTest: TDepthTest> = record
  private
    class procedure Factorize(
      const AVectorA, AVectorB, AVectorC: TFloat4;
      AAttributeA, AAttributeB, AAttributeC: PSingle;
      AStepA, AStepB, AStepD: PSingle;
      var AVecZ: TFloat3); static; inline;

    class procedure InitFactors(
        const ATarget: PSingle;
        const ABase: PSingle;
        const AMultiplier: Single;
        const AAdd: PSingle
      ); static; inline;

    class procedure StepFactors(
        ATarget: PSingle;
        AStep: PSingle
      ); static; inline;

    class procedure DenormalizeFactors(
        ATarget: PSingle;
        ASource: PSingle;
        AZ: Single
      ); static; inline;

    class procedure RenderFullBlock(
      AX, AY: Integer;
      const AStepA, AStepB, AStepD: TAttributes;
      const AZValues: TFloat3;
      AShader: Shader;
      const AFirstPixel: PRGB32;
      const ALineLength: NativeInt;
      const AFirstDepth: PSingle); static;
    class procedure RenderHalfBlock(
      APosY: PTrianglePosition;
      AFixedDeltas: PHalfSpaceDeltas;
      AX, AY: Integer;
      const AStepA, AStepB, AStepD: TAttributes;
      const AZValues: TFloat3;
      AShader: Shader;
      const AFirstPixel: PRGB32;
      const ALineLength: NativeInt;
      const AFirstDepth: PSingle); static;
    class procedure InterpolateAttributes(AX, AY: Integer; ATarget, AStepA, AStepB, AStepD: PSingle; const AZValue: Single); static; inline;
  public
    class procedure RasterizeTriangle(
      AMaxResolutionX, AMaxResolutionY: Integer;
      const AVerctorA, AvectorB, AvectorC: TFloat4;
      const AAttributesA, AAttributesB, AAttributesC: Pointer;
      AShader: Shader;
      APixelBuffer: PRGB32Array;
      ADepthBuffer: PDepthsBuffer;
      ABlockOffset, ABlockStep: Integer); static; inline;
  end;

  PPSingle = ^PSingle;

procedure EvalHalfspace(const AConstants: PHalfEdgeConstants; const ADeltas: PHalfSpaceDeltas; ACorners: PBlockCorners; AState: PBlockState);
procedure DenormalizeFactors4(ATarget, ASource: PSingle; AZ: Single);
procedure InitFactors4(
        const ATarget: PSingle;
        const ABase: PSingle;
        const AMultiplier: Single;
        const AAdd: PSingle
      ); stdcall;
procedure InterpolateAttributes4(const AX, AY: Single; ATarget, AStepA, AStepB, AStepD: PSingle; AZ: Single);
procedure StepFactors4(ATarget, AStep: PSingle);

implementation

uses
  Types;

{$PointerMath ON}
{$B+}

procedure DenormalizeFactors4(ATarget, ASource: PSingle; AZ: Single);
asm
  movups xmm0, [ASource]
  movss xmm1, [AZ]
  shufps xmm1, xmm1, 0
  rcpps xmm1, xmm1
  mulps xmm0, xmm1
  movups [ATarget], xmm0
end;

class procedure TRasterizerFactory<TAttributes, Shader, DepthTest>.DenormalizeFactors(ATarget,
  ASource: PSingle; AZ: Single);
begin
  if SizeOf(TAttributes) > 0 then
    DenormalizeFactors4(ATarget, ASource, AZ);
end;

class procedure TRasterizerFactory<TAttributes, Shader, DepthTest>.Factorize(
  const AVectorA, AVectorB, AVectorC: TFloat4;
  AAttributeA, AAttributeB, AAttributeC: PSingle;
  AStepA, AStepB, AStepD: PSingle;
  var AVecZ: TFloat3);
var
  LAW, LBW, LCW: Single;
  LStepCZ: Single;
  i: Integer;
begin
  LAW := AVectorA.W;
  LBW := AVectorB.W;
  LCW := AVectorC.W;
  LStepCZ := CalculateFactorC(AVectorA, AVectorB, AVectorC);
  if LStepCZ <> 0 then
    LStepCZ := 1 / LStepCZ
  else
    LStepCZ := 1;
  AVecZ.X := CalculateFactorA(AVectorA, AVectorB, AVectorC, 1/LAW, 1/LBW, 1/LCW) * LStepCZ;
  AVecZ.Y := CalculateFactorB(AVectorA, AVectorB, AVectorC, 1/LAW, 1/LBW, 1/LCW) * LStepCZ;
  AVecZ.Z := CalculateFactorD(AVectorA, AVectorB, AVectorC, 1/LAW, 1/LBW, 1/LCW) * LStepCZ;

  for i := 0 to Pred(SizeOf(TAttributes) div SizeOf(Single)) do
  begin
    AStepA[i] := CalculateFactorA(AVectorA, AVectorB, AVectorC, AAttributeA[i]/LAW, AAttributeB[i]/LBW, AAttributeC[i]/LCW) * LStepCZ;
    AStepB[i] := CalculateFactorB(AVectorA, AVectorB, AVectorC, AAttributeA[i]/LAW, AAttributeB[i]/LBW, AAttributeC[i]/LCW) * LStepCZ;
    AStepD[i] := CalculateFactorD(AVectorA, AVectorB, AVectorC, AAttributeA[i]/LAW, AAttributeB[i]/LBW, AAttributeC[i]/LCW) * LStepCZ;
  end;
end;

procedure InitFactors4(
        const ATarget: PSingle;
        const ABase: PSingle;
        const AMultiplier: Single;
        const AAdd: PSingle
      );
asm
//  mov eax, [AMultiplier]
  movss xmm0, [AMultiplier]
//  //set all parts of xmm1 to the value in the lowest part of xmm1
  shufps xmm0, xmm0, 0
  mov eax, [ABase];
  movups xmm1, [eax]
  mov eax, [AAdd]
  movups xmm2, [eax]
  mulps xmm1, xmm0
  addps xmm1, xmm2
  mov eax, ATarget
  movups [eax], xmm1
end;

class procedure TRasterizerFactory<TAttributes, Shader, DepthTest>.InitFactors(
        const ATarget: PSingle;
        const ABase: PSingle;
        const AMultiplier: Single;
        const AAdd: PSingle
      );
begin
  if SizeOf(TAttributes) > 0 then
    InitFactors4(ATarget, ABase, AMultiplier, AAdd);
end;

//InitFactors<TAttributes>(@LAttributesY, @LStepB, i, @LStepD);
//                      LDenormalizeZY := LStepsZ.Y * i + LStepsZ.Z;
//                      InitFactors<TAttributes>(@LAttributesX, @LStepA, k, @LAttributesY);
//                      LDenormalizeZX := LStepsZ.X * k + LDenormalizeZY;
//                      DenormalizeFactors<TAttributes>(@LAttributesDenormalized, @LAttributesX, LDenormalizeZX);
procedure InterpolateAttributes4(const AX, AY: Single; ATarget, AStepA, AStepB, AStepD: PSingle; AZ: Single);
asm
  push eax
  //initfactor StepB
  movups xmm0, [AStepB]
  movss xmm1, [AY]
  shufps xmm1, xmm1, 0
  mulps xmm0, xmm1
  mov eax, [AStepD]
  movups xmm1, [eax]
  addps xmm0, xmm1
  //initfactor StepA
  movups xmm2, [AStepA]
//  mov eax, ptr dword AX
  movss xmm1, [ebp+$14]
  shufps xmm1, xmm1, 0
  mulps xmm2, xmm1
  addps xmm2, xmm0
  //denormalize
  movss xmm1, [AZ]
  shufps xmm1, xmm1, 0
  rcpps xmm1, xmm1
  mulps xmm2, xmm1
  pop eax
  movups [ATarget], xmm2
end;

class procedure TRasterizerFactory<TAttributes, Shader, DepthTest>.InterpolateAttributes(AX, AY: Integer; ATarget, AStepA, AStepB, AStepD: PSingle; const AZValue: Single);
var
  LX, LY: Single;
begin
  if SizeOf(TAttributes) > 0 then
  begin
    LX := AX;
    LY := AY;
    InterpolateAttributes4(LX, LY, ATarget, AStepA, AStepB, AStepD, AZValue);
  end;
end;

procedure StepFactors4(ATarget, AStep: PSingle);
asm
  movups xmm0, [ATarget]
  movups xmm1, [AStep]
  addps xmm0, xmm1
  movups [ATarget], xmm0
end;

class procedure TRasterizerFactory<TAttributes, Shader, DepthTest>.StepFactors(ATarget, AStep: PSingle);
begin
  if SizeOf(TAttributes) > 0 then
    StepFactors4(ATarget, AStep);
end;


class procedure TRasterizerFactory<TAttributes, Shader, DepthTest>.RenderFullBlock(
      AX, AY: Integer;
      const AStepA, AStepB, AStepD: TAttributes;
      const AZValues: TFloat3;
      AShader: Shader;
      const AFirstPixel: PRGB32;
      const ALineLength: NativeInt;
      const AFirstDepth: PSingle);
var
  LDenormalizeZY, LDenormalizeZX: Single;
  i, k: Integer;
  LAttributesX, LAttributesY, LAttributesDenormalized: TAttributes;
  LPixelX, LPixelY: PRGB32;
  LDepthY, LDepthX: PSingle;
begin
  InitFactors(@LAttributesY, @AStepB, AY, @AStepD);
  LDenormalizeZY := AZValues.Y * AY + AZValues.Z;
  LPixelY := AFirstPixel;
  if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
    LDepthY := AFirstDepth;
  for i := AY to AY + (CQuadSize - 1) do
  begin
    InitFactors(@LAttributesX, @AStepA, AX, @LAttributesY);
    LDenormalizeZX := AZValues.X * AX + LDenormalizeZY;
    LPixelX := LPixelY;
    if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
      LDepthX := LDepthY;
    for k := AX to AX + (CQuadSize - 1) do
    begin
      {$B-}
      if (TypeInfo(DepthTest) = TypeInfo(TNoDepth)) or (LDenormalizeZX < LDepthX^) then
      {$B+}
      begin
        DenormalizeFactors(@LAttributesDenormalized, @LAttributesX, LDenormalizeZX);
        AShader.Fragment(LPixelX, @LAttributesDenormalized);
        if TypeInfo(DepthTest) = TypeInfo(TDepthWrite) then
          LDepthX^ := LDenormalizeZX;
      end;
      StepFactors(@LAttributesX, @AStepA);
      LDenormalizeZX := LDenormalizeZX + AZValues.X;
      if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
        Inc(LDepthX);
      Inc(LPixelX);
    end;
    StepFactors(@LAttributesY, @AStepB);
    LDenormalizeZY := LDenormalizeZY + AZValues.Y;
    if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
      Inc(LDepthY, ALineLength);
    Inc(LPixelY, ALineLength);
  end;
end;


class procedure TRasterizerFactory<TAttributes, Shader, DepthTest>.RenderHalfBlock(
  APosY: PTrianglePosition; AFixedDeltas: PHalfSpaceDeltas; AX, AY: Integer;
  const AStepA, AStepB, AStepD: TAttributes; const AZValues: TFloat3;
  AShader: Shader; const AFirstPixel: PRGB32; const ALineLength: NativeInt;
  const AFirstDepth: PSingle);
var
  LPixelX, LPixelY: PRGB32;
  LDepthX, LDepthY: PSingle;
  BlockXEnd, BlockYEnd: Integer;
  LX, LY: TTrianglePosition;
  i, k: Integer;
  LDenormalizedZ: Single;
  LAttributesDenormalized: TAttributes;
begin
  LPixelY := AFirstPixel;
  if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
    LDepthY := AFirstDepth;
  BlockYEnd := AY + (CQuadSize - 1);
  BlockXEnd := AX + (CQuadSize - 1);
  for i := AY to BlockYEnd do
  begin
    LX._1 := APosY._1;
    LX._2 := APosy._2;
    LX._3 := APosY._3;
    LPixelX := LPixelY;
    if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
      LDepthX := LDepthY;
    for k := AX to BlockXEnd do
    begin
      if (LX._1 or LX._2 or LX._3) >= 0 then
      begin
        LDenormalizedZ := ((AZValues.Y*i + AZValues.Z) + AZValues.X * k);
        {$B-}
        if (TypeInfo(DepthTest) = TypeInfo(TNoDepth)) or (LDenormalizedZ < LDepthX^) then
        {$B+}
        begin
          InterpolateAttributes(k, i, @LAttributesDenormalized, @AStepA, @AStepB, @AStepD, LDenormalizedZ);
          AShader.Fragment(LPixelX, @LAttributesDenormalized);
          if TypeInfo(DepthTest) = TypeInfo(TDepthWrite) then
            LDepthX^ := LDenormalizedZ;
        end;
      end;

      LX._1 := LX._1 - AFixedDeltas.Y12;
      LX._2 := LX._2 - AFixedDeltas.Y23;
      LX._3 := LX._3 - AFixedDeltas.Y31;
      Inc(LPixelX);
      if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
        Inc(LDepthX);
    end;

    LY._1 := LY._1 + AFixedDeltas.X12;
    LY._2 := LY._2 + AFixedDeltas.X23;
    LY._3 := LY._3 + AFixedDeltas.X31;
    Inc(LPixelY, ALineLength);
    if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
      Inc(LDepthY, ALineLength);
  end;
end;

procedure EvalHalfspace(const AConstants: PHalfEdgeConstants; const ADeltas: PHalfSpaceDeltas; ACorners: PBlockCorners; AState: PBlockState);
var
  A00, A10, A01, A11, B00, B10, B01, B11, C00, C10, C01, C11: Boolean;
  ResultOrA, ResultAndA, ResultOrB, ResultAndB, ResultOrC, ResultAndC: Boolean;
begin
    // Evaluate half-space functions
  a00 := (AConstants.C1 + ADeltas.X12 * ACorners.Y0 - ADeltas.Y12 * ACorners.X0) > 0;
  a10 := (AConstants.C1 + ADeltas.X12 * ACorners.Y0 - ADeltas.Y12 * ACorners.X1) > 0;
  a01 := (AConstants.C1 + ADeltas.X12 * ACorners.Y1 - ADeltas.Y12 * ACorners.X0) > 0;
  a11 := (AConstants.C1 + ADeltas.X12 * ACorners.Y1 - ADeltas.Y12 * ACorners.X1) > 0;
  ResultOrA := a00 or a10 or a01 or a11;
  ResultAndA := a00 and a10 and a01 and a11;

  b00 := (AConstants.C2 + ADeltas.X23 * ACorners.Y0 - ADeltas.Y23 * ACorners.X0) > 0;
  b10 := (AConstants.C2 + ADeltas.X23 * ACorners.Y0 - ADeltas.Y23 * ACorners.X1) > 0;
  b01 := (AConstants.C2 + ADeltas.X23 * ACorners.Y1 - ADeltas.Y23 * ACorners.X0) > 0;
  b11 := (AConstants.C2 + ADeltas.X23 * ACorners.Y1 - ADeltas.Y23 * ACorners.X1) > 0;
  ResultOrB := B00 or B10 or B01 or B11;
  ResultAndB := B00 and B10 and B01 and B11;

  c00 := (AConstants.C3 + ADeltas.X31 * ACorners.Y0 - ADeltas.Y31 * ACorners.X0) > 0;
  c10 := (AConstants.C3 + ADeltas.X31 * ACorners.Y0 - ADeltas.Y31 * ACorners.X1) > 0;
  c01 := (AConstants.C3 + ADeltas.X31 * ACorners.Y1 - ADeltas.Y31 * ACorners.X0) > 0;
  c11 := (AConstants.C3 + ADeltas.X31 * ACorners.Y1 - ADeltas.Y31 * ACorners.X1) > 0;
  ResultOrC := C00 or C10 or C01 or C11;
  ResultAndC := C00 and C10 and C01 and C11;

  AState.Intersects := ResultOrA or ResultOrB or ResultOrC;
  AState.IsFullBlock := ResultAndA and ResultAndB and ResultAndC;
end;

class procedure TRasterizerFactory<TAttributes, Shader, DepthTest>.RasterizeTriangle(
      AMaxResolutionX, AMaxResolutionY: Integer;
      const AVerctorA, AvectorB, AvectorC: TFloat4;
      const AAttributesA, AAttributesB, AAttributesC: Pointer;
      AShader: Shader;
      APixelBuffer: PRGB32Array;
      ADepthBuffer: PDepthsBuffer;
      ABlockOffset, ABlockStep: Integer);
var
  Y1, Y2, Y3, X1, X2, X3: Integer;
  MinX, MinY, MAxX, MAxY, BlockX, BlockXEnd, BlockY, BlockYEnd: Integer;
  i, k: Integer;
  LStepA, LStepB, LStepD: TAttributes;
  LStepsZ: TFloat3;
  LAttributesX, LAttributesY, LAttributesDenormalized: TAttributes;
  LDenormalizedZ: Single;
  LLineLength: NativeInt;
  LFirstPixel, LPixelX, LPixelY: PRGB32;
  LFirstDepth, LDepthX, LDepthY: PSingle;
  LStepsPerQuad: NativeInt;
  LConstants: THalfEdgeConstants;
  LDeltas, LFixedDeltas: THalfSpaceDeltas;
  LBlockCorners: TBlockCorners;
  LBlockState: TBlockState;
  LX, LY: TTrianglePosition;
begin
  //calculate attribute factors
  Factorize(AVerctorA, AvectorB, AvectorC, AAttributesA, AAttributesB, AAttributesC, @LStepA, @LStepB, @LStepD, LStepsZ);
  LLineLength := -(AMaxResolutionX+1);
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
    LDeltas.X12 := X1 - X2;
    LDeltas.X23 := X2 - X3;
    LDeltas.X31 := X3 - X1;

    LDeltas.Y12 := Y1 - Y2;
    LDeltas.Y23 := Y2 - Y3;
    LDeltas.Y31 := Y3 - Y1;

    // Fixed-point deltas
    LFixedDeltas.X12 := LDeltas.X12*16;// shl 4;
    LFixedDeltas.X23 := LDeltas.X23*16;// shl 4;
    LFixedDeltas.X31 := LDeltas.X31*16;// shl 4;

    LFixedDeltas.Y12 := LDeltas.Y12*16;// shl 4;
    LFixedDeltas.Y23 := LDeltas.Y23*16;// shl 4;
    LFixedDeltas.Y31 := LDeltas.Y31*16;// shl 4;

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
    MinX := MinX div (CQuadSize) * CQuadSize;
    MinY := MinY div (CQuadSize) * CQuadSize;
        //align to block matching stepping
    LStepsPerQuad := CQuadSize*ABlockStep;
    MinY := MinY div LStepsPerQuad * LStepsPerQuad + ABlockOffset*CQuadSize;

    // Half-edge constants
    LConstants.C1 := LDeltas.Y12 * X1 - LDeltas.X12 * Y1;
    LConstants.C2 := LDeltas.Y23 * X2 - LDeltas.X23 * Y2;
    LConstants.C3 := LDeltas.Y31 * X3 - LDeltas.X31 * Y3;

    // Correct for fill convention
    if(LDeltas.Y12 < 0) or ((LDeltas.Y12 = 0) and (LDeltas.X12 > 0))then
    begin
      LConstants.C1 := LConstants.C1 + 1;
    end;
    if(LDeltas.Y23 < 0) or ((LDeltas.Y23 = 0) and (LDeltas.X23 > 0))then
    begin
      LConstants.C2 := LConstants.C2 + 1;
    end;
    if(LDeltas.Y31 < 0) or ((LDeltas.Y31 = 0) and (LDeltas.X31 > 0))then
    begin
      LConstants.C3 := LConstants.C3 + 1;
    end;

    // Loop through blocks
    BlockY := MinY;
    while BlockY < MaxY do
    begin
      BlockX := MinX;
        while BlockX < MaxX do
        begin
            // Corners of block
            LBlockCorners.X0 := BlockX*16;// shl 4;
            LBlockCorners.X1 := (BlockX + CQuadSize - 1)*16;// shl 4;
            LBlockCorners.Y0 := BlockY*16;// shl 4;
            LBlockCorners.Y1 := (BlockY + CQuadSize - 1)*16;// shl 4;

            EvalHalfspace(@LConstants, @LDeltas, @LBlockCorners, @LBlockState);
            // Skip block when outside an edge
            if LBlockState.Intersects then
            begin
              //calculate first pixel of block
              LFirstPixel := @APixelBuffer[BlockY*LLineLength + BlockX];
              if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
                LFirstDepth := @ADepthBuffer[BlockY*LLineLength + BlockX];
              // Accept whole block when totally covered
              if LBlockState.IsFullBlock  then
              begin
                RenderFullBlock(BlockX, BlockY, LStepA, LStepB, LStepD, LStepsZ, AShader, LFirstPixel, LLineLength, LFirstDepth);
              end
              else //Partially covered Block
              begin

                LY._1 := LConstants.C1 + LDeltas.X12 * LBlockCorners.Y0 - LDeltas.Y12 * LBlockCorners.X0;
                LY._2 := LConstants.C2 + LDeltas.X23 * LBlockCorners.Y0 - LDeltas.Y23 * LBlockCorners.X0;
                LY._3 := LConstants.C3 + LDeltas.X31 * LBlockCorners.Y0 - LDeltas.Y31 * LBlockCorners.X0;

                RenderHalfBlock(@LY, @LFixedDeltas, BlockX, BlockY, LStepA, LStepB, LStepD, LStepsZ, AShader, LFirstPixel, LLineLength, LFirstDepth);
              end;
            end;
          BlockX := BlockX + CQuadSize;
        end;
      BlockY := BlockY + LStepsPerQuad;
    end;
end;

end.
