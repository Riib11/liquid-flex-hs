// inspired by https://docs.tangramflex.io/flex/start
module ExampleModule

// This is a comment

const pi: float64 = 3.14;

variant VariantA {
  Simple;
  Complex(int8, float32);
}

enum Color int32 {
  Red = 1;
  Green = 2;
  Blue = 3;
  Yellow = 4;
}

message MsgA {
  x: int32;
  flag: bit;
  assert(flag ==> 0 < x);
}

message MsgB extends MsgA {
  y: int32;
  assert(flag ==> x <= y);
}

transform f(a: MsgA): MsgB {
  MsgB{
    x = a.x;
    y = if a.flag then x*x else 0;
    flag = a.flag
  }
}

function halfInt32(x: int32): int32 {
  assert(x % 2 == 0);
  x / 2;
}


