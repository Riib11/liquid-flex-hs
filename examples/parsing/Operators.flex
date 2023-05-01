module Operators where

function f() -> bit {
    let _ = x * x;
    let _ = x / x;
    let _ = x / x * x;
    let _ = x + x / x * x - x;
    let _ = x + x / x * x - x < x;
    let _ = x + x / x * x - x <= x;
    let _ = x + x / x * x - x >= x;
    let _ = x + x / x * x - x > x;
    let _ = ! x + x / x * x - x < x;
    let _ = ! x + x / x * x - x < x && x;
    let _ = ! x + x / x * x - x < x || x;
    let _ = ! x + x / x * x - x < x || x && x;
    let _ = ! x + x / x * x - x < x || x && x == x;
    let _ = ! x + x / x * x - x < x || x && x != x;
    let _ = ! x + x / x * x - x < x || x && x != x ==> x;
    true
}