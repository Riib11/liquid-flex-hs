module Variants where

variant Bool {
    True;
    False;
}

function main() -> bit {
    let _: Bool = True;
    let _: Bool = False;
    true
}