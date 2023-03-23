module Structures where

struct Foo {
    bar: bit;
    biz: bit;
}

function main() -> bit {
    // let s1 = Foo { bar = true; biz = true };
    // let s2 = Foo { bar = false; biz = false };

    // assert s1 == s2;

    assert Foo { bar = true; biz = true } == Foo { bar = false; biz = false };
    
    // assert Foo { bar = true } == Foo { bar = true };

    true
}
