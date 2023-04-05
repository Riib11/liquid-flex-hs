module Parameters where

transform foo(b1: bit, b2: bit) -> bit {
    let b3 = b1;

    if ((b1 == true) && (b2 == true)) then {

        assert b1 == b2;
        assert b3 == b2;
        true

    } else {

        // assert b1 == b2; // FAIL

        if (b1 == true) then {
            // assert (b2 == true); // FAIL
            assert (b2 == false);
            assert (b3 == true);
            
            true
        } else {
            true
        }
    }
}