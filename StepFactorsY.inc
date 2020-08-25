asm
  mov eax, [AAttributes]
  movups xmm0, [eax + TBlockAttributes.StepB]
  addps xmm0, [LAttributesY]
  movups [LAttributesY], xmm0
end;
