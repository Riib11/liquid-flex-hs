module Tmp where

struct S1 {
    x1: int32;
    assert(x1 == 0);
}

transform f1(s1: S1) -> bit {
    assert(s1.x1 == 0);
    true
}

struct S2 {
    s1: S1;
}

transform f2(s2: S2) -> bit {
    assert(s2.s1.x1 == 0);
    true
}
