module MeasurementPrecision where

message struct Measurement {
  value: float64;
  assert(0.0 <= value);
  
  // error margin
  margin: float32;
  precision: Precision;
  assert(
    ((precision == Precision#Exact)       && (margin == 0.0)) ||
    ((precision == Precision#Approximate) && ((0.0 < margin) && (value > cast(margin))))
  );
}

// Some pmeasurements are perfectly exact, others are approximated.
enum Precision string {
  Exact = "exact";
  Approximate = "approximate";
}

// // Treat precise measurements as having `margin == 0.0`.
// function virtualMargin(measurement: Measurement) -> float32 {
//   match (measurement.margin) with {
//     None => 0.0;
//     Some(margin) => abs(margin);
//   }
// }

// Average of two measurements, respecting error margins.
transform average(measurement1: Measurement, measurement2: Measurement) -> Measurement {
  let valueAvg = (measurement1.value + measurement2.value) / 2.0;
  
  if ((measurement1.precision == Precision#Exact) && 
      (measurement2.precision == Precision#Exact)) 
  
  then {
    Measurement {
      value = valueAvg;
      margin = 0.0;
      precision = Precision#Exact
    }
  } 
  
  else {
    // let margin1 = virtualMargin(measurement1);
    // let margin2 = virtualMargin(measurement2);
    let marginAvg = (measurement1.margin + measurement2.margin) / 2.0;
    
    // assert 0.0 <= marginAvg; // PASSES
    // assert marginAvg <= max(measurement1.margin, measurement2.margin); // PASSES
    // assert marginAvg >= min(measurement1.margin, measurement2.margin); // PASSES

    Measurement {
      value = valueAvg;
      margin = marginAvg;
      precision = Precision#Approximate
    }
  }
}

// Gets the upper bound of a measurement as a precise measurement.
transform upperBound(measurement: Measurement) -> Measurement {
  Measurement {
    value = measurement.value + cast(measurement.margin);
    margin = 0.0;
    precision = Precision#Exact
  }
}

// utilities

function abs(x: float32) -> float32 { if (x <= 0.0) then 0.0 - x else x }
function max(x: float32, y: float32) -> float32 { if (x >= y) then x else y }
function min(x: float32, y: float32) -> float32 { if (x <= y) then x else y }