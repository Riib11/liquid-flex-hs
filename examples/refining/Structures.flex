module Structures where

struct EqualBits {
    b1: bit;
    b2: bit;
    assert b1 == b2;
}

const ebs1: EqualBits = EqualBits{ b1 = false; b2 = false }

const abs2: bit = {
    let ebs = EqualBits{ b1 = false; b2 = false };
    let b = ebs.b1;
    assert !b;
    assert ebs.b1 == ebs.b2;
    assert !ebs.b1 == !ebs.b2;
    true
}

// FAIL
// const abs3: EqualBits = EqualBits{ b1 = true; b2 = false }

// struct A {
//     x: int32;
//     y: int32;
//     assert (x < y); // !TODO implement primitive `<`
// }

struct NonemptyString {
    content: string;
    assert !(content == "");
}

function equalInt32(x: int32, y: int32) -> bit {
    x == y
}

struct EqualInt32s {
    x1: int32;
    x2: int32;
    assert equalInt32(x1, x2);
}
