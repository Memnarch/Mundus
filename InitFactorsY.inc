asm
  movss xmm0, [LY]
//  //set all parts of xmm0 to the value in the lowest part of xmm1
  shufps xmm0, xmm0, 0

  movups xmm1, xmm4
  mulps xmm1, xmm0
  movups xmm2, dqword ptr [LStepD]//[eax + TBlockAttributes.StepD]
  addps xmm1, xmm2
  movups xmm6, xmm1

  {$if SizeOf(TAttributes) >= 32}
  mov eax, [AAttributes]
  movups xmm1, [eax + TBlockAttributes.StepB + 16]
  mulps xmm1, xmm0
  addps xmm1, [eax + TBlockAttributes.StepD + 16]
  movups dqword ptr [LFactorY + 16], xmm1
  {$ifend}
end;