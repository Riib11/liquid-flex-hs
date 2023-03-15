module Variants where

variant Bool {
    True;
    False;
}

function main() -> bit {
    let _: Bool = Bool#True();
    let _: Bool = Bool#False();
    true
}