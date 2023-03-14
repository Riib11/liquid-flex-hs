module Transforms where

message M1 {
    x: int32;
}

transform f1(m1: M1) -> bit {
    true
}