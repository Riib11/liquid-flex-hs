//
// numeric types, cast
//


let y: int64 = foo(...);
// assert(0 <= y && y < 2^32);
let x: uint32 = cast(y);


//
// struct, properties, extension
//

struct Nonnegative { x: int32; assert(1 <= x); }
// predicate: isNonnegative(nn)
// axiom: isNonnegative(nn) <==> 1 <= nn.x

struct Even extends Nonnegative { assert(x % 2 == 0); }
// predicate: isEven(ev)
// axiom: isEven(ev) <==> (ev.x % 2 == 0) && isNonnegative(ev)

transform half(ev: Even) -> Nonnegative {
  // assert(isNonnegative({ x = ev.x / 2 }))
  Nonnegative { x = ev.x / 2 } }


struct Nonnegative 
  { x: int32; 
    assert(1 <= x); }
// axiom: isNonnegative(nn) <==> 
//        1 <= nn.x

struct Even extends Nonnegative
  { assert(x % 2 == 0); }
// axiom: isEven(ev) <==> 
//        (ev.x % 2 == 0) && 
//        isNonnegative(ev)

transform half(ev: Even) -> Nonnegative {
  // assume isEven(ev)
  // assert isNonnegative({ x = ev.x / 2 })
  Nonnegative { x = ev.x / 2 } 
}

//
// variant, pattern matching
//

enum Status bit { Ok = true; Error = false; }

struct Result {
  value: int8; status: Status;
  assert
    (((status == Status#Ok) 
        && (0 <= value)) ||
     ((status == Status#Error) 
        && (value == -1)));
}

function fromOk(result: Result) -> Optional<uint8> {
  match(result.status) {
    Status#Ok => {
      // assume result.status == Status#Ok
      // assert 0 <= result.value < 2^8
      Some(cast(result.value))
    }
    Status#Error => None
  }
  // assert exhaustive match
}


