{$IF SizeOf(TAttributes) >= 16}//4 Values
asm
  movss xmm0, [LDenormalizeZX]
  shufps xmm0, xmm0, 0
  rcpps xmm0, xmm0

  movups xmm1, xmm7
  mulps xmm1, xmm0
  movups dqword ptr [LAttributesDenormalized], xmm1
  {$IF SizeOf(TAttributes) >= 32}
  movups xmm1, dqword ptr [LFactorX+16]
  mulps xmm1, xmm0
  movups dqword ptr [LAttributesDenormalized+16], xmm1
  {$ifend}
end;
{$ifend}

{$IF SizeOf(TAttributes) > 32}
  {$Message Error 'Unsupported Shader Attributesize'}
{$ifend}
