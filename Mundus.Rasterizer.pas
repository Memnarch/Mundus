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
  type
    TBlockAttributes = packed record
      X, Y: Integer;
      StepA, StepB, StepD: TAttributes;
      ZValues: TFloat3;
      ShaderInstance: Shader;
      FirstPixel: PRGB32;
      LineLength: NativeInt;
      FirstDepth: PSingle
    end;
    PBlockAttributes = ^TBlockAttributes;
  private
    class procedure Factorize(
      const AVectorA, AVectorB, AVectorC: TFloat4;
      AAttributeA, AAttributeB, AAttributeC: PSingle;
      AStepA, AStepB, AStepD: PSingle;
      var AVecZ: TFloat3); static; inline;

    class procedure RenderFullBlock(const AAttributes: PBlockAttributes); static;
    class procedure RenderHalfBlock(
      APosY: PTrianglePosition;
      AFixedDeltas: PHalfSpaceDeltas;
      const AAttributes: PBlockAttributes); static;
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

procedure InitFactors(
        const ATarget: PSingle;
        const ABase: PSingle;
        const AMultiplier: Single;
        const AAdd: PSingle;
        const AAttributeSize: Integer
      ); inline;

procedure DenormalizeFactors(
        ATarget: PSingle;
        ASource: PSingle;
        AZ: Single;
        AAttributeSize: Integer
      ); inline;

procedure StepFactors(
        AStep: PSingle;
        ATarget: PSingle;
        const AAttributeSize: Integer
      ); inline;
procedure InterpolateAttributes4(const AX, AY: Single; ATarget, AStepA, AStepB, AStepD: PSingle; AZ: Single);
procedure StepFactors4(AStep, ATarget: PSingle);
procedure InterpolateAttributes(AX, AY: Integer; ATarget, AStepA, AStepB, AStepD: PSingle; const AZValue: Single; const AAttributeSize: Integer); inline;

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

procedure DenormalizeFactors(
        ATarget: PSingle;
        ASource: PSingle;
        AZ: Single;
        AAttributeSize: Integer
      );
begin
  if AAttributeSize > 0 then
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

procedure InitFactors(
        const ATarget: PSingle;
        const ABase: PSingle;
        const AMultiplier: Single;
        const AAdd: PSingle;
        const AAttributeSize: Integer
      );
begin
  if AAttributeSize > 0 then
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

procedure InterpolateAttributes(AX, AY: Integer; ATarget, AStepA, AStepB, AStepD: PSingle; const AZValue: Single; const AAttributeSize: Integer);
begin
  if AAttributeSize > 0 then
    InterpolateAttributes4(AX, AY, ATarget, AStepA, AStepB, AStepD, AZValue);
end;

procedure StepFactors4(AStep, ATarget: PSingle);
asm
  movups xmm0, [ATarget]
  movups xmm1, [AStep]
  addps xmm1, xmm0
  movups [ATarget], xmm1
end;

procedure StepFactors(
        AStep: PSingle;
        ATarget: PSingle;
        const AAttributeSize: Integer
      );
begin
  if AAttributeSize > 0 then
    StepFactors4(AStep, ATarget);
end;


class procedure TRasterizerFactory<TAttributes, Shader, DepthTest>.RenderFullBlock(const AAttributes: PBlockAttributes);
var
  LDenormalizeZY, LDenormalizeZX: Single;
  i, k: Integer;
  LAttributesX, LAttributesY, LAttributesDenormalized: TAttributes;
  LPixelX, LPixelY: PRGB32;
  LDepthY, LDepthX: PSingle;
begin
  InitFactors(@LAttributesY, @AAttributes.StepB, AAttributes.Y, @AAttributes.StepD, SizeOf(TAttributes));
  LDenormalizeZY := AAttributes.ZValues.Y * AAttributes.Y + AAttributes.ZValues.Z;
  LPixelY := AAttributes.FirstPixel;
  if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
    LDepthY := AAttributes.FirstDepth;
  for i := AAttributes.Y to AAttributes.Y + (CQuadSize - 1) do
  begin
    InitFactors(@LAttributesX, @AAttributes.StepA, AAttributes.X, @LAttributesY, SizeOf(TAttributes));
    LDenormalizeZX := AAttributes.ZValues.X * AAttributes.X + LDenormalizeZY;
    LPixelX := LPixelY;
    if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
      LDepthX := LDepthY;
    for k := AAttributes.X to AAttributes.X + (CQuadSize - 1) do
    begin
      {$B-}
      if (TypeInfo(DepthTest) = TypeInfo(TNoDepth)) or (LDenormalizeZX < LDepthX^) then
      {$B+}
      begin
        DenormalizeFactors(@LAttributesDenormalized, @LAttributesX, LDenormalizeZX, SizeOf(TAttributes));
        AAttributes.ShaderInstance.Fragment(LPixelX, @LAttributesDenormalized);
        if TypeInfo(DepthTest) = TypeInfo(TDepthWrite) then
          LDepthX^ := LDenormalizeZX;
      end;
      StepFactors(@AAttributes.StepA, @LAttributesX, SizeOf(TAttributes));
      LDenormalizeZX := LDenormalizeZX + AAttributes.ZValues.X;
      if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
        Inc(LDepthX);
      Inc(LPixelX);
    end;
    StepFactors(@AAttributes.StepB, @LAttributesY, SizeOf(TAttributes));
    LDenormalizeZY := LDenormalizeZY + AAttributes.ZValues.Y;
    if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
      Inc(LDepthY, AAttributes.LineLength);
    Inc(LPixelY, AAttributes.LineLength);
  end;
end;


class procedure TRasterizerFactory<TAttributes, Shader, DepthTest>.RenderHalfBlock(
      APosY: PTrianglePosition;
      AFixedDeltas: PHalfSpaceDeltas;
      const AAttributes: PBlockAttributes);
var
  LPixelX, LPixelY: PRGB32;
  LDepthX, LDepthY: PSingle;
  BlockXEnd, BlockYEnd: Integer;
  LX, LY: TTrianglePosition;
  i, k: Integer;
  LDenormalizedZ: Single;
  LAttributesDenormalized: TAttributes;
begin
  LPixelY := AAttributes.FirstPixel;
  if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
    LDepthY := AAttributes.FirstDepth;
  BlockYEnd := AAttributes.Y + (CQuadSize - 1);
  BlockXEnd := AAttributes.X + (CQuadSize - 1);
  for i := AAttributes.Y to BlockYEnd do
  begin
    LX._1 := APosY._1;
    LX._2 := APosy._2;
    LX._3 := APosY._3;
    LPixelX := LPixelY;
    if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
      LDepthX := LDepthY;
    for k := AAttributes.X to BlockXEnd do
    begin
      if (LX._1 or LX._2 or LX._3) >= 0 then
      begin
        LDenormalizedZ := ((AAttributes.ZValues.Y*i + AAttributes.ZValues.Z) + AAttributes.ZValues.X * k);
        {$B-}
        if (TypeInfo(DepthTest) = TypeInfo(TNoDepth)) or (LDenormalizedZ < LDepthX^) then
        {$B+}
        begin
          InterpolateAttributes(k, i, @LAttributesDenormalized, @AAttributes.StepA, @AAttributes.StepB, @AAttributes.StepD, LDenormalizedZ, SizeOf(TAttributes));
          AAttributes.ShaderInstance.Fragment(LPixelX, @LAttributesDenormalized);
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
    Inc(LPixelY, AAttributes.LineLength);
    if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
      Inc(LDepthY, AAttributes.LineLength);
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
  MinX, MinY, MAxX, MAxY, BlockXEnd, BlockYEnd: Integer;
  i, k: Integer;
  LAttributesX, LAttributesY, LAttributesDenormalized: TAttributes;
  LDenormalizedZ: Single;
  LStepsPerQuad: NativeInt;
  LConstants: THalfEdgeConstants;
  LDeltas, LFixedDeltas: THalfSpaceDeltas;
  LBlockCorners: TBlockCorners;
  LBlockState: TBlockState;
  LX, LY: TTrianglePosition;
  LBlock: TBlockAttributes;
begin
  //calculate attribute factors
  Factorize(AVerctorA, AvectorB, AvectorC, AAttributesA, AAttributesB, AAttributesC, @LBlock.StepA, @LBlock.StepB, @LBlock.StepD, LBlock.ZValues);
  LBlock.LineLength := -(AMaxResolutionX+1);
  LBlock.ShaderInstance := AShader;
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
    LBlock.ShaderInstance.MinX := minx;
    LBlock.ShaderInstance.MinY := miny;


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
    LBlock.Y := MinY;
    while LBlock.Y < MaxY do
    begin
      LBlock.X := MinX;
        while LBlock.X < MaxX do
        begin
            // Corners of block
            LBlockCorners.X0 := LBlock.X*16;// shl 4;
            LBlockCorners.X1 := (LBlock.X + CQuadSize - 1)*16;// shl 4;
            LBlockCorners.Y0 := LBlock.Y*16;// shl 4;
            LBlockCorners.Y1 := (LBlock.Y + CQuadSize - 1)*16;// shl 4;

            EvalHalfspace(@LConstants, @LDeltas, @LBlockCorners, @LBlockState);
            // Skip block when outside an edge
            if LBlockState.Intersects then
            begin
              //calculate first pixel of block
              LBlock.FirstPixel := @APixelBuffer[LBlock.Y*LBlock.LineLength + LBlock.X];
              if TypeInfo(DepthTest) <> TypeInfo(TNoDepth) then
                LBlock.FirstDepth := @ADepthBuffer[LBlock.Y*LBlock.LineLength + LBlock.X];
              // Accept whole block when totally covered
              if LBlockState.IsFullBlock  then
              begin
                RenderFullBlock(@LBlock);
              end
              else //Partially covered Block
              begin

                LY._1 := LConstants.C1 + LDeltas.X12 * LBlockCorners.Y0 - LDeltas.Y12 * LBlockCorners.X0;
                LY._2 := LConstants.C2 + LDeltas.X23 * LBlockCorners.Y0 - LDeltas.Y23 * LBlockCorners.X0;
                LY._3 := LConstants.C3 + LDeltas.X31 * LBlockCorners.Y0 - LDeltas.Y31 * LBlockCorners.X0;

                RenderHalfBlock(@LY, @LFixedDeltas, @LBlock);
              end;
            end;
          LBlock.X := LBlock.X + CQuadSize;
        end;
      LBlock.Y := LBlock.Y + LStepsPerQuad;
    end;
end;

end.
