//XMM7 represents LAttributesX
//XMM6 represents LAttributesY
asm
  mov eax, [AAttributes]
  movups xmm5, [eax + TBlockAttributes.StepA]
  movups xmm4, [eax + TBlockAttributes.StepB]
  CVTSI2SS xmm0, [eax + TBlockAttributes.X]
  movss [LX], xmm0
  CVTSI2SS xmm0, [eax + TBlockAttributes.Y]
  movss [LY], xmm0
end;