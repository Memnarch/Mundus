{$IF SizeOf(TAttributes) = 16}//4 Values
asm
  movss xmm0, [LDenormalizeZX]
  shufps xmm0, xmm0, 0
  rcpps xmm0, xmm0
  mulps xmm0, [LAttributesX]
  movups [LAttributesDenormalized], xmm0
end;
{$else}
{$if SizeOf(TAttributes) > 0}
{$Message Error 'Unsupported Shader Attributesize'}
{$ifend}
{$ifend}