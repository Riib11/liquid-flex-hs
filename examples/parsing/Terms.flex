// NOTE: maltyped

module Terms where

const x: bit = {
    // literal 
    let _ = 1;
    let _ = 111111111;
    let _ = 1111.2222;

    // basic
    let _ = s.x;
    let _ = s.x.y.z.w;
    let _ = t : T;
    let _ = t : T1 : T2;
    let _ = (x);
    let _ = (((((x)))));
    let _ = { x };
    let _ = { let x = 1; let y = 2; x };
    let _ = cast(x);
    let _ = cast x;
    let _ = try(x);
    let _ = try x;
    let _ = if x then y else z;

    // construct structure
    let _ = S {};
    let _ = S { x = 1 };
    let _ = S { x = 1; y = 2; z = 3 };

    // construct variant
    let _ = V();
    let _ = V(true, false, 3);

    // construct enum
    let _ = E1;

    // construct newtype
    let _ = N(x);

    // function application
    let _ = f();
    let _ = f(x, y, z);
    let _ = f(x, y, z) giving (a, b, c);

    // prefix and infix
    let _ = !x;
    let _ = x && y;
    let _ = !x && !y;
    let _ = x || y;
    let _ = !x || !y && !z;
    let _ = !x || !y && !z == !z && !y || !x;

    // block
    let _ = { x };
    let _ = {{ x }};
    let _ = {{{ x }}};
    let _ = { assert(x); x };
    let _ = { assert({{ x }}); x };
    
    // pattern
    let _ = { let x = y; x };
    let _ = { let _ = x; x };
    let _ = { let 1 = x; x };

    // result of block
    true
}