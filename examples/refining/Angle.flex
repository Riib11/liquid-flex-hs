// !TODO look into how LF reasons about reals
// !TODO reason about floats _as if_ they were reals, not accounting for inaccuracy introduced by actual floating point arith

module Angle where

// A degree is an angle measurement unit, where 360 deg = 0 deg. This struct
// encodes the equivalence classes of degrees, so the value `d` must be in the
// range `0 < d <= 360`.
struct Degrees {
    d: float32;
    assert (0.0 <= d && d < 360.0);
}

transform oppositeDegrees(degrees: Degrees) -> Degrees {
    if (degrees.d < 180.0) then {
        Degrees{ d = degrees.d + 180.0 }
    } else {
        Degrees{ d = degrees.d - 180.0 }
    }
}

const pi: float32 = 3.14159

// A radian is an angle measurement unit, where 2π rad is equal to 0 rad. 
// This struct encodes the equivalent classes of radians, so the value `r` must 
// be in the range from `0 <= r < 2 * π`.
struct Radians {
    r: float32;
    assert (0.0 <= r && r < 2.0*pi);
}

// d deg => ((d / 360) * 2π) rad
transform fromDegreesToRadians(degrees: Degrees) -> Radians {
    // Note that we don't have to use modulus here, because `Degrees`'s 
    // refinement (along with some primitive arithmetic knowledge) implies the 
    // `r` satisfies `Radians`'s refinement'.
    Radians{ r = (degrees.d / 360.0) * (2.0 * pi) }
}

// r rag => ((r / 2π) * 360) deg
transform fromRadiansToDegrees(radians: Radians) -> Degrees {
    Degrees{ d = (radians.r / (2.0 * pi)) * 360.0 }
}
