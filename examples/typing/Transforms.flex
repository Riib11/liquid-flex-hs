module Transforms where

message M1 {
    x: int32;
    b: bit;
}

transform f1(m1: M1) -> bit {
    m1.b
}