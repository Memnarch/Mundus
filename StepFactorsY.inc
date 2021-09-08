asm

  addps xmm6, xmm4
  {$if SIzeOf(TAttributes) >= 32}
  mov eax, [AAttributes]
  movups xmm0, dqword ptr [LFactorY + 16]
  addps xmm0, [eax + TBlockAttributes.StepB + 16]
  movups dqword ptr [LFactorY + 16], xmm0
  {$ifend}
end;
