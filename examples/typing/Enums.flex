module Enums where

enum Bit int1 {
    zero = 0;
    one = 1;
}

function main() -> bit {
    let _: Bit = zero;
    let _: Bit = one;
    let _: bit = zero == one;
    true
}