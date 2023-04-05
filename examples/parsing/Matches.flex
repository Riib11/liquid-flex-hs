module Matches where

variant Coin {
    Heads;
    Tails;
}

// no parameters `V3()` is the same as empty parameters `V4`
variant V {
    V1(bit);
    V2(int32, int64, int128);
    V3();
    V4;
}

enum E int32 {
    E1 = 1;
    E2 = 2;
    E3 = 3;
}

function main() -> bit {
    let v1 = V1(true);
    match v1 with {
        V#V1(b) => true;
        V#V2(x, y, z) => false;
        V#V3() => false;
        V#V4() => false;
    }
}