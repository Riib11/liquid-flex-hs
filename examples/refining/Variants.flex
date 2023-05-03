module Variants where

variant Coin { 
    Head();
    Tail();
}

const ex1: bit = {
    let c1 = Coin#Head();
    let c2 = Coin#Head();
    let c3 = Coin#Tail();
    assert c1 == c2;
    assert !(c1 == c3);

    true
}

variant Nat {
   Zero();
   Suc(Nat);
}

const ex2: bit = {
    let zero = Nat#Zero();
    let one = Nat#Suc(zero);

    let one' = Nat#Suc(Nat#Zero());

    assert one == one';

    true
}