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