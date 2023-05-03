module Locals where

transform f1() -> bit {
  let a = true;
  let b = true;
  assert a == b;
  
  true
}

transform f2() -> bit {
  let a = { let x = 1; x };
  assert a == 1;

  true
}