module Discriminant where

message MaybeTaggedUnion {
    discriminant: int8;
    x1: Optional<int32>;
    x2: Optional<string>;
    x3: Optional<Tuple<int8, bit>>;
    assert(
        ((discriminant == 1) && (x1 != None) && (x2 == None) && (x3 == None)) ||
        ((discriminant == 2) && (x1 == None) && (x2 != None) && (x3 == None)) ||
        ((discriminant == 3) && (x1 == None) && (x2 == None) && (x3 != None)) ||
        ((discriminant == 4) && (x1 == None) && (x2 == None) && (x3 == None))
    );
}
