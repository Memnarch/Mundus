asm

  addps xmm7, xmm5
  {$if SIzeOf(TAttributes) >= 32}
  mov eax, [AAttributes]
  movups xmm0, dqword ptr [LFactorX + 16]
  addps xmm0, [eax + TBlockAttributes.StepA + 16]
  movups dqword ptr [LFactorX + 16], xmm0
  {$ifend}
end;