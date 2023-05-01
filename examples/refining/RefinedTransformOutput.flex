module RefinedTransformOutput where

struct S1 {
    x: int32;
    y: int32;
    assert(x == 0);
}

transform f1() -> S1 {
    S1 {x = 0; y = 10}
}

transform f2() -> bit {
    let s1 = f1();
    assert s1.x == 0;
    true
}

struct S2 {
    s1: S1;
    assert(s1.y == 1);
}

transform f3() -> bit {
    let s1 = f1();
    // let s2 = S2 {s1 = s1}; // FAIL
    true
}