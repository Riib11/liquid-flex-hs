module Enums where

enum Bit int1 {
    zero = 0;
    one = 1;
}

function main() -> bit {
    let _: Bit = Bit#zero;
    let _: Bit = Bit#one;
    let _: bit = Bit#zero == Bit#one;
    true
}