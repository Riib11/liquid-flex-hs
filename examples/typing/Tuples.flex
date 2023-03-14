module Tuples where

function main() -> bit {
    let _: Tuple<int32, bit> = (1, true);
    let _: Tuple<Tuple<int32, bit>, bit> = ((1, true), false);
    let _: Tuple<Array<bit>, Array<int32>> = ([true, false], [0, 1]);
    true
}