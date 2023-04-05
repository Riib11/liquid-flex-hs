module Matches where


variant V {
    V1(bit);
    V2(int32, int64, int128);
    V3();
    V4;
}

transform test1() -> bit {
    let v1 = V#V1(true);
    assert v1 == V#V1(true);
    true
}

transform test2(v : V) -> bit {
    match v with {
        V#V1(b) => true;
        V#V2(x, y, z) => {
            if (x == x) then {
                assert x == x;
                true
            } else true
        };
        V#V3() => false;
        V#V4() => false;
    }
}

transform test3(v1 : V, v2 : V) -> bit {
    if (v1 == v2) then {
        match v1 with {
            V#V1(b) => {
                assert v2 == V#V1(b);
                true
            };
            V#V2(x, y, z) => false;
            V#V3() => false;
            V#V4() => false;
        }
    } else false
}

transform test4(o : Optional<bit>) -> bit {
    match o with {
        None => false;
        Some(b) => b;
    }
}

variant Coin {
    Heads;
    Tails;
}

transform test5(c : Coin) -> bit {
    match c with {
        Coin#Heads => true;
        Coin#Tails => false;
    }
}

transform test6(c : Coin) -> bit {
    let b = 
        match c with {
            Coin#Heads => true;
            Coin#Tails => false;
        };
    b
}