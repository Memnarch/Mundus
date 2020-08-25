asm
  mov eax, [AAttributes]
  movups xmm0, [eax + TBlockAttributes.StepA]
  addps xmm0, [LAttributesX]
  movups [LAttributesX], xmm0
end;