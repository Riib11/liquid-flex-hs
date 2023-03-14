// NOTE: maltyped

module Declarations where

struct S1 {
    x: int32;
}

struct S2 extends S1 {
    y: int32;
    z: int32;
    assert(x == y);
    assert(x == z);
}

message S3 extends S2 {
    b: bit;
    assert(b);
}

newtype N {
    b: bit;
    assert(b == b || !b);
}

variant V {
    V1(bit);
    V2(int32, int64, int128);
    V3();
    V4;
}

enum E int32 {
    E1 = 1;
    E2 = 2;
    E3 = 3;
}

type A = char

function f(x: int32, y: bit, z: char) given (?a: int32) -> int32 {
    let w = 1;
    w
}

transform g(s3: S3) given (?n: N) -> S3 {
    s3
}

const c: int32 = 1
