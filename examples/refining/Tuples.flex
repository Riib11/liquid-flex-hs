module Tuples where

function main() -> bit {
    assert {
        let t = (1, 2);
        t == (1, 3)
    };

    true
}
