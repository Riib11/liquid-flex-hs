module OrderedPair where

message OrderedPair {
    x: int32;
    y: int32;
    assert(x <= y);
}

