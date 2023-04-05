module Functions where 

function assertIsZero(x: int32) -> bit {
    assert x == 0;
    true
}

transform main(x: int32) -> bit {
    // let _ = assertIsZero(x); // FAIL

    if (x == 0) then {
        let _ = assertIsZero(x);
        true
    } else {
        true
    }
}