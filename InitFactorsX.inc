asm
  movss xmm0, [LX]
//  //set all parts of xmm0 to the value in the lowest part of xmm1
  shufps xmm0, xmm0, 0
//  mov eax, [ABase];
  movups xmm1, xmm5
  mulps xmm1, xmm0
  addps xmm1, xmm6
  movups xmm7, xmm1
end;