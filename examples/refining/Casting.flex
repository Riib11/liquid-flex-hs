module Casting where

transform f1() -> bit {
    let x16: int16 = 1;
    let x32: int32 = 1;
    let x64: int64 = 1;
    let u32: uint32 = 1;

    assert((cast(x32)) == x64);
    assert((cast(x32)) == x16);

    assert((cast(u32)) == x64);
    assert((cast(u32)) == x32);
    assert((cast(u32)) == x16);
    true
}

transform f2() -> bit {
    let f16: float16 = 1.0;
    let f32: float32 = 1.0;
    let f64: float64 = 1.0;

    assert((cast(f32)) == f64);
    assert((cast(f32)) == f16);

    true
}