{$IF SizeOf(TAttributes) >= 16}//4 Values
asm
  //calculate denormalized z for later
  movss xmm3, [LDenormalizedZ]
  shufps xmm3, xmm3, 0
  rcpps xmm3, xmm3

  //first 16
  //load i
  CVTSI2SS xmm1, [i]
  shufps xmm1, xmm1, 0
  //initfactor StepB
  mov eax, [AAttributes]
  movups xmm0, [eax + TBlockAttributes.StepB]
  mulps xmm0, xmm1
  //StepD
  addps xmm0, [eax + TBlockAttributes.StepD]
  //initfactor StepA
  //get k to xmm1
  CVTSI2SS xmm1, [k]
  shufps xmm1, xmm1, 0
  movups xmm2, [eax + TBlockAttributes.StepA]
  mulps xmm2, xmm1
  addps xmm0, xmm2
  //denormalize
  mulps xmm0, xmm3
  movups dqword ptr [eax + TBlockAttributes.AttributesDenormalized], xmm0

  //next 16
  {$if SizeOf(TAttributes) >= 32}
    //load i
  CVTSI2SS xmm1, [i]
  shufps xmm1, xmm1, 0
  //initfactor StepB
  movups xmm0, [eax + TBlockAttributes.StepB+16]
  mulps xmm0, xmm1
  //StepD
  addps xmm0, [eax + TBlockAttributes.StepD+16]
  //initfactor StepA
  //get k to xmm1
  CVTSI2SS xmm1, [k]
  shufps xmm1, xmm1, 0
  movups xmm2, [eax + TBlockAttributes.StepA+16]
  mulps xmm2, xmm1
  addps xmm0, xmm2
  //denormalize
  mulps xmm0, xmm3
  movups dqword ptr [eax + TBlockAttributes.AttributesDenormalized + 16], xmm0
  {$ifend}
  //next 16
  {$if SizeOf(TAttributes) >= 48}
    //load i
  CVTSI2SS xmm1, [i]
  shufps xmm1, xmm1, 0
  //initfactor StepB
  movups xmm0, [eax + TBlockAttributes.StepB+32]
  mulps xmm0, xmm1
  //StepD
  addps xmm0, [eax + TBlockAttributes.StepD+32]
  //initfactor StepA
  //get k to xmm1
  CVTSI2SS xmm1, [k]
  shufps xmm1, xmm1, 0
  movups xmm2, [eax + TBlockAttributes.StepA+32]
  mulps xmm2, xmm1
  addps xmm0, xmm2
  //denormalize
  mulps xmm0, xmm3
  movups dqword ptr [eax + TBlockAttributes.AttributesDenormalized + 32], xmm0
  {$ifend}

  //next 16
  {$if SizeOf(TAttributes) >= 64}
    //load i
  CVTSI2SS xmm1, [i]
  shufps xmm1, xmm1, 0
  //initfactor StepB
  movups xmm0, [eax + TBlockAttributes.StepB+48]
  mulps xmm0, xmm1
  //StepD
  addps xmm0, [eax + TBlockAttributes.StepD+48]
  //initfactor StepA
  //get k to xmm1
  CVTSI2SS xmm1, [k]
  shufps xmm1, xmm1, 0
  movups xmm2, [eax + TBlockAttributes.StepA+48]
  mulps xmm2, xmm1
  addps xmm0, xmm2
  //denormalize
  mulps xmm0, xmm3
  movups dqword ptr [eax + TBlockAttributes.AttributesDenormalized + 48], xmm0
  {$ifend}
end;
{$ifend}

{$if SizeOf(TAttributes) > 64}
{$Message Error 'Unsupported Shader Attributesize'}
{$ifend}
