asm
  movss xmm0, [LY]
//  //set all parts of xmm0 to the value in the lowest part of xmm1
  shufps xmm0, xmm0, 0
//  mov eax, [ABase];
  movups xmm1, xmm4
  mulps xmm1, xmm0
//  mov eax, [AAttributes]
  movups xmm0, [LStepD]//[eax + TBlockAttributes.StepD]
  addps xmm1, xmm0
  movups xmm6, xmm1
end;