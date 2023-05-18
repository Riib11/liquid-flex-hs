module DivideByZero where

transform main() -> bit {
    // let _: int32 = 1 / 1; // PASSES

    // let _: int32 = 1 / 0; // FAILS

    // let _: int32 = 1 / sub1_opaque(1); // FAILS

    // let _: int32 = 1 / sub1_opaque(2); // FAILS

    // let _: int32 = 1 / sub1_transparent(1); // FAILS

    // let _: int32 = 1 / sub1_transparent(2); // PASSES

    true
}

transform sub1_opaque(x: int32) -> int32 {
    x - 1
}

function sub1_transparent(x: int32) -> int32 {
    x - 1
}