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

procedure RenderFullBlock(const AAttributes: PBlockAttributes);
var
  LDenormalizeZY, LDenormalizeZX: Single;
  i, k: Integer;
  LAttributesX, LAttributesY, LAttributesDenormalized: TAttributes;
  LPixelX, LPixelY: PRGB32;
  LDepthY, LDepthX: PSingle;
begin
  InitFactors(@AAttributes.StepB, AAttributes.Y, @LAttributesY, @AAttributes.StepD, SizeOf(TAttributes));
  LDenormalizeZY := AAttributes.ZValues.Y * AAttributes.Y + AAttributes.ZValues.Z;
  LPixelY := AAttributes.FirstPixel;
  if DepthTest <> dtNone then
    LDepthY := AAttributes.FirstDepth;
  for i := AAttributes.Y to AAttributes.Y + (CQuadSize - 1) do
  begin
    InitFactors(@AAttributes.StepA, AAttributes.X, @LAttributesX, @LAttributesY, SizeOf(TAttributes));
    LDenormalizeZX := AAttributes.ZValues.X * AAttributes.X + LDenormalizeZY;
    LPixelX := LPixelY;
    if DepthTest <> dtNone then
      LDepthX := LDepthY;
    for k := AAttributes.X to AAttributes.X + (CQuadSize - 1) do
    begin
      {$B-}
      if (DepthTest = dtNone) or (LDenormalizeZX <= LDepthX^) then
      {$B+}
      begin
        DenormalizeFactors(@LAttributesDenormalized, @LAttributesX, LDenormalizeZX, SizeOf(TAttributes));
        AAttributes.ShaderInstance.Fragment(LPixelX, @LAttributesDenormalized);
        if DepthTest = dtWrite then
          LDepthX^ := LDenormalizeZX;
      {$IFDEF ShowZFail}
      end
      else
      begin
        LPixelX.R := 200;
      {$EndIF}
      end;
      StepFactors(@AAttributes.StepA, @LAttributesX, SizeOf(TAttributes));
      LDenormalizeZX := LDenormalizeZX + AAttributes.ZValues.X;
      if DepthTest <> dtNone then
        Inc(LDepthX);
      Inc(LPixelX);
    end;
    StepFactors(@AAttributes.StepB, @LAttributesY, SizeOf(TAttributes));
    LDenormalizeZY := LDenormalizeZY + AAttributes.ZValues.Y;
    if DepthTest <> dtNone then
      Inc(LDepthY, AAttributes.LineLength);
    Inc(LPixelY, AAttributes.LineLength);
  end;
end;


procedure RenderHalfBlock(
      APosY: PTrianglePosition;
      AFixedDeltas: PHalfSpaceDeltas;
      const AAttributes: PBlockAttributes);
var
  LDepthX, LDepthY: PSingle;
  BlockXEnd, BlockYEnd: Integer;
  LX: TTrianglePosition;
  i, k: Integer;
  LDenormalizedZ: Single;
  LAttributesDenormalized: TAttributes;
begin
  if DepthTest <> dtNone then
    LDepthY := AAttributes.FirstDepth;
  BlockYEnd := AAttributes.Y + (CQuadSize - 1);
  BlockXEnd := AAttributes.X + (CQuadSize - 1);
  //abuse padding fields to store Pixel-Pointer and increment it along with our X/Y position
  AFixedDeltas._XPadding := AAttributes.LineLength*SizeOf(TRGB32);
  AFixedDeltas._YPadding := -SizeOf(TRGB32);
  APosY._Padding := Integer(AAttributes.FirstPixel);
  //LX, LY, DeltaX, DeltaY will be kept in sse registers
  asm
    mov eax, APosY
    movdqu xmm6, [eax];
    mov eax, [AFixedDeltas]
    movdqu xmm5, [eax + THalfSpaceDeltas.X12]
    movdqu xmm4, [eax + THalfSpaceDeltas.Y12]
  end;
  for i := AAttributes.Y to BlockYEnd do
  begin
    asm movdqu xmm7, xmm6 end;
    if DepthTest <> dtNone then
      LDepthX := LDepthY;
    for k := AAttributes.X to BlockXEnd do
    begin
      asm movdqu [LX], xmm7 end;
      if (LX._1 or LX._2 or LX._3) >= 0 then
      begin
        LDenormalizedZ := ((AAttributes.ZValues.Y*i + AAttributes.ZValues.Z) + AAttributes.ZValues.X * k);
        {$B-}
        if (DepthTest = dtNone) or (LDenormalizedZ <= LDepthX^) then
        {$B+}
        begin
          InterpolateAttributes(k, i, @LAttributesDenormalized, @AAttributes.StepA, @AAttributes.StepB, @AAttributes.StepD, LDenormalizedZ, SizeOf(TAttributes));
          AAttributes.ShaderInstance.Fragment(PRGB32(LX._Padding), @LAttributesDenormalized);
          if DepthTest = dtWrite then
            LDepthX^ := LDenormalizedZ;
        {$IFDEF ShowZFail}
        end
        else
        begin
          PRGB32(LX._Padding).R := 200;
        {$ENDIF}
        end;
      end;

      asm psubd xmm7, xmm4 end;
      if DepthTest <> dtNone then
        Inc(LDepthX);
    end;

    asm paddd xmm6, xmm5 end;
    if DepthTest <> dtNone then
      Inc(LDepthY, AAttributes.LineLength);
  end;
end;

procedure RasterizeTriangle(
      AMaxResolutionX, AMaxResolutionY: Integer;
      const AVerctorA, AvectorB, AvectorC: TFloat4;
      const AAttributesA, AAttributesB, AAttributesC: Pointer;
      AShader: Shader;
      APixelBuffer: PRGB32Array;
      ADepthBuffer: PDepthsBuffer;
      ALowDepthBuffer: PDepthsBuffer;
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
  LMinDepth, LMaxDepth: Single;
  LFirstLowDepth: PSingle;
  LLowDepthLineLength: NativeInt;
begin
  LMinDepth := Min(AVerctorA.Z, Min(AvectorB.Z, AvectorC.Z));
  LMaxDepth := Max(AVerctorA.Z, Max(AvectorB.Z, AvectorC.Z));
  LLowDepthLineLength := - (((AMaxResolutionX+1) + 7) div 8);
  //calculate attribute factors
  Factorize(AVerctorA, AvectorB, AvectorC, AAttributesA, AAttributesB, AAttributesC, @LBlock.StepA, @LBlock.StepB, @LBlock.StepD, LBlock.ZValues, SizeOf(TAttributes));
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
            if DepthTest <> dtNone then
            begin
              LBlock.FirstDepth := @ADepthBuffer[LBlock.Y*LBlock.LineLength + LBlock.X];
              LFirstLowDepth := @ALowDepthBuffer[(LBlock.Y div CQuadSize) * LLowDepthLineLength + (LBlock.X div 8)];
              if LFirstLowDepth^ < LMinDepth then
              begin
                LBlock.X := LBlock.X + CQuadSize;
                Continue;
              end;
            end;
            // Accept whole block when totally covered
            if LBlockState.IsFullBlock  then
            begin
              RenderFullBlock(@LBlock);
              if DepthTest = dtWrite then
                LFirstLowDepth^ := LMaxDepth;
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