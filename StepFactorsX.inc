asm

  addps xmm7, xmm5
  {$if SIzeOf(TAttributes) >= 32}
  mov eax, [AAttributes]
  movups xmm0, dqword ptr [LFactorX + 16]
  addps xmm0, [eax + TBlockAttributes.StepA + 16]
  movups dqword ptr [LFactorX + 16], xmm0
  {$ifend}

  {$if SIzeOf(TAttributes) >= 48}
  movups xmm0, dqword ptr [LFactorX + 32]
  addps xmm0, [eax + TBlockAttributes.StepA + 32]
  movups dqword ptr [LFactorX + 32], xmm0
  {$ifend}


  {$if SIzeOf(TAttributes) >= 64}
  movups xmm0, dqword ptr [LFactorX + 48]
  addps xmm0, [eax + TBlockAttributes.StepA + 48]
  movups dqword ptr [LFactorX + 48], xmm0
  {$ifend}
end;

{$IF SizeOf(TAttributes) > 64}
  {$Message Error 'Unsupported Shader Attributesize'}
{$ifend}