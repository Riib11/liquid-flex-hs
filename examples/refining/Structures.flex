module Structures where

struct Point32 {
    x: int32;
    y: int32;
}

struct EqualBits {
    b1: bit;
    b2: bit;
    assert b1 == b2;
}

function main() -> bit {

    // Point32

    let s1 = Point32{ x = 0; y = 0 };
    let s2 = Point32{ x = 0; y = 0 };
    let s3 = Point32{ x = 0; y = 1 };

    // assert  (s1 == s2);
    // assert !(s3 == s1);
    // assert !(s3 == s2);

    // assert !(Point32{ x = 0; y = 0 } == Point32 { x = 1; y = 1 });

    // // EqualBits

    // let eb1 = EqualBits{ b1 = true; b2 = false };

    true
}
