module Coordinates where

const pi: float32 = 3.14

struct Angle {
  radians: float32;
  assert(0.0 <= radians && radians <= 2.0*pi);
}

// Polar coordinates encode coordinates by an angle from the horizon and a
// radius along that angle. 
struct Polar {
  theta: Angle;
  r: float32;
  assert(0.0 <= r);
  // Specify a normal form for `theta` when `r = 0`, since by default 
  // the zero coordinate can be encoded by any theta if `r = 0`.
  assert(r == 0.0  ==>  theta.radians == 0.0);
}

struct Cartesian {
  x: float32;
  y: float32;
}

transform fromPolarToCartesian(polar: Polar) -> Cartesian {
  Cartesian{ 
    x = polar.r * cos(polar.theta.radians);
    y = polar.r * sin(polar.theta.radians)
  }
}

transform fromCartesianToPolar(cartesian: Cartesian) -> Polar {
  if (cartesian.y == 0.0) then {
    Polar{
      theta = Angle{ radians = if (cartesian.x >= 0.0) then 0.0 else pi };
      r = abs(cartesian.x)
    }
    // if (cartesian.x >= 0.0) then {
    //   Polar{ 
    //     theta = Angle{ radians = 0.0 }; 
    //     r = cartesian.x
    //   }
    // } else {
    //   Polar{ 
    //     theta = Angle{ radians = pi }; 
    //     r = neg(1.0) * cartesian.x
    //   }
    // }
  } else {
    Polar{
      theta = arctan(cartesian.x / cartesian.y).angle;
      r = sqrt(sq(cartesian.x) + sq(cartesian.y))
    }
  }
}

// utilities

// fake
transform sin(x: float32) -> float32 { x }

// fake
transform cos(x: float32) -> float32 { x }

//
//       /|
//      / |
//     /  |
//  c /   |
//   /    | a
//  / θ   |
// --------
//   b
//
// tan(θ) = a / b
// c² = a² + b² 
//
struct ArcTan {
  angle: Angle;
  assert(angle.radians != pi/2.0);
}
// fake
transform arctan(x: float32) -> ArcTan {
  ArcTan{ angle = Angle{ radians = 0.0 } }
}

// fake
function sqrt(x: float32) -> float32 { x }

function sq(x: float32) -> float32 { x * x }

function neg(x: float32) -> float32 { 0.0 - x }

function signum(x: float32) -> float32 {
  if (x <  0.0) then neg(1.0) 
  else if (x == 0.0) then 0.0 
  else 1.0
}

function abs(x: float32) -> float32 {
  if (x >= 0.0) then x else neg(1.0) * x
}