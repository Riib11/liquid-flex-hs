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

// Since `sub1_opaque` is a transform, its implementation details are treated as 
// opaque by refinement types. Only the type of the output of a transform is 
// known during refinement checking.
transform sub1_opaque(x: int32) -> int32 { x - 1 }

// Since `sub1_transparent` is a (non-transform) function, its implementation 
// details are reflected into refinement types. I.e. functions are inlined 
// during refinement checking.
function sub1_transparent(x: int32) -> int32 { x - 1 }

