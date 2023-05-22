module Exceptions where

// const e_int32: int32 = assertFalse
// const e_float32: float32 = assertFalse
// const e_bit: bit = assertFalse

struct Positive {
  x: int32;
  assert 0 < x;
}

transform f(p: Positive) -> bit {
  if (0 < p.x) then {
    true
  } else {
    assertFalse
  }
}