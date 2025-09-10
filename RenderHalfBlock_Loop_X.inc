//ahhhhh what do we want to do here?
//We need to check if X or Y or Z are negative (vecor in xmm7). Which equals this condition
//if (LX._1 or LX._2 or LX._3) >= 0 then
//This concludes, we are interested in the high byte of each integer which contains the sign bit, only
//therefore we set every byte we don't want to 0 using the Byteshuffle with our mask
//afterwards we create a bitmask which has a bit set for each byte which has its sign bit set (byte vectors are signed)
//if eax is 0, we know that all of our values were >= 0, if not, we need to skip this block
asm
  movdqu xmm2, xmm7 //make copy to work nondestructive
  movdqu xmm3, CSignByteMask //load mask
  pshufb xmm2, xmm3 //zero all lower 3 bytes of each integer using mask
  pmovmskb eax, xmm2 //create bitmask with bit for every negative byte
  cmp eax, 0 //ehck if yn bit is set and jump to Blockmissed if any is
  jnz @BlockMissed

  //LDenormalizedZ := ((LZValues.Y*i + LZValues.Z) + LZValues.X * k);
  CVTSI2SS xmm0, i
  movups xmm2, LZValues
  movaps xmm3, xmm2
  shufps xmm3, xmm3, 1 + 2 shl 2
  mulss xmm0, xmm3
  shufps xmm3, xmm3, 1
  addss xmm0, xmm3
  CVTSI2SS xmm1, k
  mulss xmm1, xmm2
  addss xmm0, xmm1

  //if (DepthTest = dtNone) or (LDenormalizedZ <= LDepthX^) then
  {$IF DepthTest <> dtNone}
    mov eax, [LDepth]
    movss xmm1, [eax]
    ucomiss xmm1, xmm0
    jb @ZFail
  {$ENDIF}
  {$IF DepthTest = dtWrite}
    //LDepthX^ := LDenormalizedZ;
    movss [eax], xmm0
  {$ENDIF}
  {$I InterpolateAttributes.inc}
  {$IF declared(FragmentShader)}
  lea ecx, [eax + TBlockAttributes.AttributesDenormalized]
  mov eax, [LConstantValues]
  //edx = TTrianglePosition._Padding
  extractps edx, xmm7, 3
  call FragmentShader
  {$ENDIF}
  @ZFail:
  @BlockMissed:

  psubd xmm7, xmm4
  {$IF DepthTest <> dtNone}
  add [LDepth], CSizeOfSingle
  {$ENDIF}
end;
