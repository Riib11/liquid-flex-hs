module Datatypes where

newtype Degrees {
  d: float32;
  assert (0.0 <= d && d < 360.0);
}

newtype Radians {
  r: float32;
  assert (0.0 <= r && r < 2.0*pi);
}

transform fromDegreesToRadians(degrees: Degrees) -> Radians {
  Radians{ r = (degrees.d / 360.0) * (2.0 * pi) }
}

transform fromRadiansToDegrees(radians: Radians) -> Degrees {
  Degrees{ d = (radians.r / (2.0 * pi)) * 360.0 }
}

variant AngleMeasurement {
  DegreeMeasurement(Degree);
  RadianMeasurement(Radians);
}

enum Status string {
  Ok = "Ok";
  Unstable = "Unstable";
  Error = "Error";
}
