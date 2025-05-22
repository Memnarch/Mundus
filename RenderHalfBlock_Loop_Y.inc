asm movdqu xmm7, xmm6 end;
if DepthTest <> dtNone then
  LDepthX := LDepthY;

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
if DepthTest <> dtNone then
  Inc(LDepthY, AAttributes.LineLength);