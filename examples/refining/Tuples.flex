module Tuples where

transform main() -> bit {
    assert   (1, 2, 3) == (1, 2, 3);
    assert !((1, 2, 3) == (1, 2, 4));

    let t = (1, 2, 3);
    assert   t == (1, 2, 3);
    assert !(t == (1, 2, 4));
    
    // assert (t == (1, 2, 4)); // FAIL

    true
}

transform foo(b: bit) -> bit {
    assert b == b;
    assert !b == !b;
    b
}