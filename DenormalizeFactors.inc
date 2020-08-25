{$IF SizeOf(TAttributes) = 16}//4 Values
asm
  movups xmm0, [LAttributesX]
  movss xmm1, [LDenormalizeZX]
  shufps xmm1, xmm1, 0
  rcpps xmm1, xmm1
  mulps xmm0, xmm1
  movups [LAttributesDenormalized], xmm0
end;
{$else}
{$if SizeOf(TAttributes) > 0}
{$Message Error 'Unsupported Shader Attributesize'}
{$ifend}
{$ifend}