module Casting where

transform f1() -> bit {
    let x16: int16 = 1;
    let x32: int32 = 1;
    let x64: int64 = 1;
    assert((cast x32) == x64);
    assert((cast x32) == x16);
    true
}