module StructureConstructors where 

struct EqualTo1 {
    x: int32;
    assert x == 1;
}

transform foo() -> bit {
    let _ = EqualTo1 { x = 1 };

    true
}