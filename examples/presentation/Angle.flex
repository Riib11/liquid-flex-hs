module Angle where

// inclusive min, exclusive max
function inBounds(min: float32, value: float32, max: float32) -> bit {
    min <= value && value < max
}

struct Degrees {
    d: float32;
    assert inBounds(0.0, d, 360.0);
}

// uninterpreted predicate: isDegrees(degrees)
// axiom: forall degrees . isDegrees(degrees) ==> inBounds(0.0, degrees.d, 360.0)

transform oppositeDegrees(degrees: Degrees) -> Degrees {
    if (degrees.d < 180.0) then {
        Degrees{ d = degrees.d + 180.0 }
    } else {
        Degrees{ d = degrees.d - 180.0 }
    }
}

const pi: float32 = 3.14159

struct Radians {
    r: float32;
    assert inBounds(0.0, r, 2.0*pi);
}

// uninterpreted predicate: isRadians(radians)
// axiom: forall radians . isRadians(radians) ==> inBounds(0.0, radians.r, 2.0*pi)

// d deg => ((d / 360) * 2π) rad
transform fromDegreesToRadians(degrees: Degrees) -> Radians {
    // Asserts inBounds(0.0, (degrees.d / 360.0) * (2.0 * pi), 2.0*pi).
    // No need to use modulus in order to pass.
    Radians { r = (degrees.d / 360.0) * (2.0 * pi) }
}

// r rag => ((r / 2π) * 360) deg
transform fromRadiansToDegrees(radians: Radians) -> Degrees {
    // Asserts inBounds(0.0, (radians.r / (2.0 * pi)) * 360.0, 360.0).
    Degrees { d = (radians.r / (2.0 * pi)) * 360.0 }
}
