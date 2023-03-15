module RefiningTest1 where

const b: bit = true

function main() -> bit {
    let x = 1;
    let y = 1;
    assert(x == y);
    true
}

// function main() -> bit {
//     let x = true;
//     let y = false;
//     let z: int32 = 1;
//     assert(z == z);
//     assert(x && !y);
//     true
// }
