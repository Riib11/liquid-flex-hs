module Tuples where

function main() -> bit {
    // (true, false, true)

    assert   (1, 2, 3) == (1, 2, 3);
    assert !((1, 2, 3) == (1, 2, 4));

    // TODO: activate this once local variables are handled properly
    // let t = (1, 2, 3);
    // assert   t == (1, 2, 3);
    // assert !(t == (1, 2, 4));

    true
}
