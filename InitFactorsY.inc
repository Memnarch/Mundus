asm
  mov eax, [AAttributes]
  CVTSI2SS xmm0, [eax + TBlockAttributes.Y]
//  //set all parts of xmm0 to the value in the lowest part of xmm1
  shufps xmm0, xmm0, 0
//  mov eax, [ABase];
  movups xmm1, [eax + TBlockAttributes.StepB]
  mulps xmm1, xmm0
  movups xmm0, [eax + TBlockAttributes.StepD]
  addps xmm1, xmm0
  movups [LAttributesY], xmm1
end;