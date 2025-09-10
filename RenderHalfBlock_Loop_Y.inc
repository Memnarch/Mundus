asm movdqu xmm7, xmm6 end;

//loop from BlockXStart to BlockXEnd
k := BlockXStart;
{$i RenderHalfBlock_Loop_X.inc}
Inc(k);
{$i RenderHalfBlock_Loop_X.inc}
Inc(k);
{$i RenderHalfBlock_Loop_X.inc}
Inc(k);
{$i RenderHalfBlock_Loop_X.inc}
Inc(k);
{$i RenderHalfBlock_Loop_X.inc}
Inc(k);
{$i RenderHalfBlock_Loop_X.inc}
Inc(k);
{$i RenderHalfBlock_Loop_X.inc}
Inc(k);
{$i RenderHalfBlock_Loop_X.inc}

asm paddd xmm6, xmm5 end;
