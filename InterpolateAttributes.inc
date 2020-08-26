{$IF SizeOf(TAttributes) = 16}//4 Values
asm
  //save _AX (eax) for later
  CVTSI2SS xmm3, [k]
  //initfactor StepB
  movups xmm0, [LStepB]
  CVTSI2SS xmm1, [i]
  shufps xmm1, xmm1, 0
  mulps xmm0, xmm1
//  mov eax, [AStepD]
  movups xmm1, [LStepD]
  addps xmm0, xmm1
  //initfactor StepA
//  mov eax, [AStepA]
  movups xmm2, [LStepA]
//  mov eax, ptr dword AX

  shufps xmm3, xmm3, 0
  mulps xmm2, xmm3
  addps xmm2, xmm0
  //denormalize
  movss xmm1, [LDenormalizedZ]
  shufps xmm1, xmm1, 0
  rcpps xmm1, xmm1
  mulps xmm2, xmm1
  movups [LAttributesDenormalized], xmm2
end;
{$else}
{$if SizeOf(TAttributes) > 0}
{$Message Error 'Unsupported Shader Attributesize'}
{$ifend}
{$ifend}