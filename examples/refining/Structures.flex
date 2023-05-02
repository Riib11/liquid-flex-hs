module Structures where

function p(b: bit) -> bit {
    b
}

struct S {
    b: bit;
    assert(p(b));
}

transform test(s: S) -> bit {
    assert(s.b);
    true
}


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

// const abs3: EqualBits = EqualBits{ b1 = true; b2 = false } // FAIL

struct NonemptyString {
    content: string;
    assert !(content == "");
}

function equalInt32(x: int32, y: int32) -> bit {
    assert !(x == y); 
    x == y
}

struct EqualInt32s {
    x1: int32;
    x2: int32;
    assert equalInt32(x1, x2);
}


transform test1(ei: EqualInt32s) -> bit {
    // assert ei.x1 == ei.x2; // FAIL !TODO assume refinements (nested) on inputs
    true
}

// transform idEqualInt32s(ie: EqualInt32s) -> EqualInt32s { ... }

// forall e . (e : EqualInt32s) => equalInt32(e.x1, e.x2)


transform test2(a1: int32, a2: int32) -> bit {
    if (a1 == a2) then {
        // let ei  = EqualInt32s { x1 = a2; x2 = a2 }; // FAIL !TODO don't reflect `let` as lambda-app, instead follow same solution as reflecting `match`
        // let ei' = idEqualInt32s(ei);
        // assert ei'.x1 == ei'.x2; // FAIL !TODO assume refinements (nested) on outputs of transforms
        true
    } else true
}
