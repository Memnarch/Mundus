asm

  addps xmm6, xmm4
  {$if SIzeOf(TAttributes) >= 32}
  mov eax, [AAttributes]
  movups xmm0, dqword ptr [LFactorY + 16]
  addps xmm0, [eax + TBlockAttributes.StepB + 16]
  movups dqword ptr [LFactorY + 16], xmm0
  {$ifend}

  {$if SIzeOf(TAttributes) >= 48}
  movups xmm0, dqword ptr [LFactorY + 32]
  addps xmm0, [eax + TBlockAttributes.StepB + 32]
  movups dqword ptr [LFactorY + 32], xmm0
  {$ifend}


  {$if SIzeOf(TAttributes) >= 64}
  movups xmm0, dqword ptr [LFactorY + 48]
  addps xmm0, [eax + TBlockAttributes.StepB + 48]
  movups dqword ptr [LFactorY + 48], xmm0
  {$ifend}
end;

{$IF SizeOf(TAttributes) > 64}
  {$Message Error 'Unsupported Shader Attributesize'}
{$ifend}
