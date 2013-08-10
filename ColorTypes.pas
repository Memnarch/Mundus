unit ColorTypes;

interface

type
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGB32)-1] of TRGB32;
  PRGB32Array = ^TRGB32Array;

  function RGB32(R, G, B, A: Byte): TRGB32;

implementation

function RGB32(R, G, B, A: Byte): TRGB32;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

end.
