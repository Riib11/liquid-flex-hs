module Tmp where

struct S1 {
    x1: int32;
    assert(x1 == 0);
}

transform foo(s1: S1) -> bit {
    assert(s1.x1 == 0);
    true
}
