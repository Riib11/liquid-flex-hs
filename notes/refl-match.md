# Relecting Match?

## Example 1: Variant

```
variant T {
    T1(bit);
    T2(int32);
}

transform foo(b : bit, x : int32, t : T) -> T {
    match t {
        case T1(b') => { assert(b == b'); t };
        case T2(x') => { assert(x == x'); t };
    }
}
```

Checking `foo`, yields this constraint

```
forall { b : bit }
forall { x : int32 }
forall { b' : bit }
forall { x' : int32 }
forall { t : T | T == T1(b') || T == T2(x') }
and
    t == T1(b') ==>
        b == b'
    t == T2(x') ==>
        x == x'
```

## Example 2: Refined Structure

```
message OrdPair {
    x: int32;
    y: int32;
    assert x <= y;
}

transform foo(op : OrdPair) -> OrdPair {
    match op {
        OrdPair x y => {
            assert x <= y; [!!]
            op
        }
    }
}

constraint at [!!]:
    // transform foo(op : OrdPair) -> ... { ... }
    forall op : OrdPair
    exists x' : int32, y' : int32
    if op == OrdPair x' y'
    if x' <= y'

    // match op { OrdPair x y => ... }
    forall x : int32, y : int32
    if op == OrdPair x y
    if x' <= y'

    // assert x <= y
    x <= y

transform bar(op : OrdPair) -> OrdPair {
    let op' = foo(op);
    // assert op'.x <= op'.y; [!!]
    assert (match op' with OrdPair x' y' => x') <= 
           (match op' with OrdPair x' y' => y')
    op'
}

constraint at [!!]:
    // transform bar(op : OrdPair) -> ... { ... }
    forall op : OrdPair
    exists op.x : int32, op.y : int32
    if op == OrdPair op.x op.y
    if op.x <= op.y


    // let op' = foo(op); ...
    exists op' : OrdPair
    forall op'.x : int32, op'.y
    if op' == OrdPair op'.x op'.y
    if op'.x <= op'.y
    if op' == foo(op)

    // assert (match op' with OrdPair x' y' => x') <= 
    //        (match op' with OrdPair x' y' => y')
    
```

Checking `foo` yields this constraint.

