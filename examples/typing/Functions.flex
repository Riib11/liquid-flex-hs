module Functions where

function f1() -> bit {
    true
}

function f2(x: int32, y: bit, z: char) -> bit {
    let _: int32 = x;
    let _: bit = y;
    let _: char = z;
    true
}

function f3() -> bit {
    f2(1, true, 'a')
}

function f4(y: bit) -> bit {
    f2(1, y, 'b')
}

function f5(?a: bit) -> bit {
    ?a
}

function f8() -> bit {
    f2(1, f2(1, true, 'a'), 'c')
}

// contextual param/args

newtype IsZero {
    x: int32;
    assert(x == 0);
}

function f6() given (?z: IsZero) -> bit {
    let _: IsZero = ?z;
    true
}

function f7() given (?z: IsZero) -> bit {
    let _: bit = f6();
    let _: bit = f6() giving (?z);
    true
}