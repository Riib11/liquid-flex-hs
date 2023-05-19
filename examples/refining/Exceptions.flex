module Exceptions where

// const e_int32: int32 = exception
// const e_float32: float32 = exception
// const e_bit: bit = exception

struct Positive {
  x: int32;
  assert 0 < x;
}

transform f(p: Positive) -> bit {
  if (0 < p.x) then {
    true
  } else {
    exception
  }
}