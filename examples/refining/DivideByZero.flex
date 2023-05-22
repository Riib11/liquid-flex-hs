module DivideByZero where

message struct Unit {}

message struct Int32 {x: int32;}

transform main(unit: Unit, i: Int32) -> Unit {
    let _: int32 = 1 / 1; // PASSES

    // let _: int32 = 1 / 0; // FAILS

    let _: int32 = 1 / i.x;

    unit
}