module Functions where 

function assertIsZero(x: int32) -> bit {
    assert x == 0;
    true
}

// transform foo(a : {A | P}) -> {B | Q} {
//     ...
// }

transform main(x: int32) -> bit {

    // let x = try {
    //     // assert P;
    //     // v
    //     foo()
    // };

    // <==>

    // if !P {
    //     None 
    // } else {
    //     Some(v)
    // }

    // let _ = assertIsZero(x); // FAIL

    if (x == 0) then {
        let _ = assertIsZero(x);
        true
    } else {
        true
    }
}

struct Different {
    dx: int32;
    dy: int32;
    assert !(dx == dy);
}

function bothNonzero(d : Different) -> bit {
    assert !(d.dx == 0);
    assert !(d.dy == 0);
    true
}

transform testDifferent() -> bit {
    let _ = bothNonzero(Different { dx = 1; dy = 2 });
    // let _ = bothNonzero(Different { dx = 0; dy = 2 }); // FAIL
    // let _ = bothNonzero(Different { dx = 1; dy = 1 }); // FAIL
    true
}