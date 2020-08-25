asm
  mov eax, [AAttributes]
  CVTSI2SS xmm0, [eax + TBlockAttributes.X]
//  //set all parts of xmm0 to the value in the lowest part of xmm1
  shufps xmm0, xmm0, 0
//  mov eax, [ABase];
  movups xmm1, [eax + TBlockAttributes.StepA]
  mulps xmm1, xmm0
  addps xmm1, [LAttributesY]
  movups [LAttributesX], xmm1
end;