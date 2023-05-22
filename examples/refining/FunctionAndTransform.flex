module DivideByZero where

// !TODO transforms must have message inputs and outputs
// this example is too technical, not intuitive

struct Value { x: int32; }

// Since `decrementOpaque` is a transform, its implementation details are
// treated as opaque by refinement types. Only the type of the output of a
// transform is known during refinement checking.
transform decrementOpaque(val: Value) -> Value { Value{ x = val.x - 1 } }

// Since `decrementTransparent` is a function, its implementation details are
// reflected into refinement types. I.e. functions are inlined during refinement
// checking.
function decrementTransparent(val: Value) -> Value { Value{ x = val.x - 1 } }

transform main() -> bit {
    let val0 = Value{ x = 1 };
    let val1 = Value{ x = 1 };
    let val2 = Value{ x = 2 };

    let _: int32 = val1.x / val1.x; // PASSES

    let _: int32 = val1.x / val0.x; // FAILS

    let _: int32 = val1.x / decrementOpaque(val1).x; // FAILS

    let _: int32 = val1.x / decrementOpaque(val2).x; // FAILS

    let _: int32 = val1.x / decrementTransparent(val1).x; // FAILS

    let _: int32 = val1.x / decrementTransparent(val2).x; // PASSES

    true
}


