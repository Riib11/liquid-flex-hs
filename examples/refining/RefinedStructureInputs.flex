module RefinedStructureInputs where

struct S1 {
    b1: bit;
    b2: bit;
    assert(b1);
}

transform f1(s1: S1) -> bit {
    assert(s1.b1);
    true
}

struct S2 {
    s1: S1;
}

transform f2(s2: S2) -> bit {
    assert(s2.s1.b1);
    true
}

struct S3 {
    s1: S1;
    assert(s1.b2);
}

transform f3(s3: S3) -> bit {
    assert(s3.s1.b2);
    true
}

struct S1' extends S1 {
    assert(b2);
}

transform f1'(s1': S1') -> bit {
    assert(s1'.b2);
    true
}