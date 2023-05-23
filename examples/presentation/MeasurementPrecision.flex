module MeasurementPrecision where

message struct Measurement {
  value: float64;
  assert(0.0 <= value);
  
  // error margin
  margin: Optional<float32>;
  precision: Precision;
  assert(
    ((precision == Precision.Exact)       && (margin == None)) ||
    ((precision == Precision.Approximate) && (margin != None))
  );
}

// Some pmeasurements are perfectly exact, others are approximated.
enum Precision string {
  Exact = "exact";
  Approximate = "approximate";
}

// Treat precise measurements as having `margin == 0.0`.
function virtualMargin(measurement: Measurement) -> float32 {
  match(measurement.margin) {
    None => 0.0;
    Some(margin) => abs(margin)
  }
}

// Average of two measurements, respecting error margins.
transform average(measurement1: Measurement, measurement2: Measurement) -> Measurement {
  let avg = (measurement1.value + measurement2.value) / 2.0;
  
  if ((measurement1.precision == Precision.Exact) && 
      (measurement2.precision == Precision.Exact)) 
  
  then {
    Measurement {
      value = avg;
      margin = None;
      precision = Precision.Exact
    }
  } 
  
  else {
    let margin1 = virtualMargin(measurement1);
    let margin2 = virtualMargin(measurement2);
    assert(0.0 <= margin1 + margin2)
    Measurement {
      value = avg;
      margin = Some((margin1 + margin2) / 2.0);
      precision = Precision.Approximate
    }
  }
}

// Gets the upper bound of a measurement as a precise measurement.
transform upperBound(measurement: Measurement) -> Measurement {
  Measurement {
    value = measurement.value + cast(virtualMargin(measurement));
    margin = None;
    precision = Precision.Exact
  }
}

// utilities

function abs(x: float32) -> float32 { if (x <= 0.0) then - x else x }
