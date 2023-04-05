module StructureConstructors where 

struct EqualTo1 {
    x: int32;
    assert x == 1;
}

transform foo() -> bit {
    let s = EqualTo1 { x = 1 };
    let y = s.x;
    assert y == 1;
    true
}