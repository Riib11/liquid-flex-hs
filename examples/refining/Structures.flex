module Structures where

struct Foo {
    bar: bit;
    biz: bit;
}

function main() -> bit {
    let s1 = Foo { bar = true; biz = true };
    let s2 = Foo { bar = true; biz = true };
    let s3 = Foo { bar = true; biz = false };

    assert  (s1 == s2);
    assert !(s3 == s1);
    assert !(s3 == s2);

    assert Foo { bar = true; biz = true } == Foo { bar = false; biz = false };

    true
}
