asm
  movss xmm0, [LX]
//  //set all parts of xmm0 to the value in the lowest part of xmm1
  shufps xmm0, xmm0, 0

  movups xmm1, xmm5
  mulps xmm1, xmm0


  addps xmm1, xmm6
  movups xmm7, xmm1
  {$if SizeOf(TAttributes) >= 32}
  mov eax, [AAttributes]
  movaps xmm1, [eax + TBlockAttributes.StepA + 16]
  mulps xmm1, xmm0
  movups xmm2, dqword ptr [LFactorY + 16]
  addps xmm1, xmm2
  movups dqword ptr [LFactorX + 16], xmm1
  {$ifend}

  {$if SizeOf(TAttributes) >= 48}
  movaps xmm1, [eax + TBlockAttributes.StepA + 32]
  mulps xmm1, xmm0
  movups xmm2, dqword ptr [LFactorY + 32]
  addps xmm1, xmm2
  movups dqword ptr [LFactorX + 32], xmm1
  {$ifend}


  {$if SizeOf(TAttributes) >= 64}
  movaps xmm1, [eax + TBlockAttributes.StepA + 48]
  mulps xmm1, xmm0
  movups xmm2, dqword ptr [LFactorY + 48]
  addps xmm1, xmm2
  movups dqword ptr [LFactorX + 48], xmm1
  {$ifend}
end;

{$IF SizeOf(TAttributes) > 64}
  {$Message Error 'Unsupported Shader Attributesize'}
{$ifend}
