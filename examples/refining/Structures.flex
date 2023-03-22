module Structures where

struct Foo {
    foo: bit;
}

function main() -> bit {
    let s1 = Foo { foo = true };
    let s2 = Foo {};
    // let s = Foo {};
    
    // assert Foo { foo = true } == Foo { foo = true };

    true
}