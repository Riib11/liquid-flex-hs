module Coordinates where

struct Angle {
  radians: float32;
  assert(0.0 <= radians && radians <= 2.0*pi);
}

// Polar coordinates are encoded by an angle from the horizon and a radius 
// in the direction of that angle. 
struct Polar {
  theta: Angle;
  r: float32;
  assert(0.0 <= r);
  // Specify a normal form for `theta` when `r = 0`, since by default 
  // the zero coordinate can be encoded by any theta if `r = 0`.
  assert(r == 0.0  ==>  theta.radians == 0.0);
}

// Cartesian coordinates are encoded by a point on the x-axis and a point on the
// y-axis.
struct Cartesian {
  x: float32;
  y: float32;
}

// Polar -> Cartesian
transform fromPolarToCartesian(polar: Polar) -> Cartesian {
  Cartesian{ 
    x = polar.r * cos(polar.theta);
    y = polar.r * sin(polar.theta)
  }
}

// Cartesian -> Polar
transform fromCartesianToPolar(cartesian: Cartesian) -> Polar {
  Polar{
    theta = 
      if (cartesian.y == 0.0) then {
        Angle{ radians = if (0.0 <= cartesian.x) then 0.0 else pi }
      } else {
        arctan(cartesian.x / cartesian.y)
      };
    r = sqrt(sq(cartesian.x) + sq(cartesian.y))
  }
}

// utilities

const pi: float32 = 3.14

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

// fake utilities

function sin(theta: Angle) -> float32 { theta.radians }

function cos(theta: Angle) -> float32 { theta.radians }

// ensures output.radians != pi/2.0
function arctan(x: float32) -> Angle {
  Angle{ radians = 0.0 }
}

function sqrt(x: float32) -> float32 {
  assert(0.0 <= x);
  x
}
