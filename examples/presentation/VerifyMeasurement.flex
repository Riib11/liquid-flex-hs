module VerifyMeasurement where

message struct Measurement {
  value: float64;
  defaultValue: float64;
}

message struct Bounds {
  min: float64;
  max: float64;
}

message struct Result {
  value: float32;
}

transform verify(bounds: Bounds, sensor: Measurement) -> Result {
  if (bounds.min <= sensor.value && sensor.value <= bounds.max) {
    Result { value = cast(sensor.value) }
  } else {
    assert(bounds <= sensor.defaultValue && sensor.defaultValue <= bounds.max);
    Result { value = cast(sensor.defaultValue) }
  }
}