module VerifyMeasurement where

message struct Measurement {
  value: float32;
  defaultValue: float32;

  minValue: float32;
  maxValue: float32;
  // assert(minValue <= defaultValue);
  // assert(defaultValue <= maxValue);
}

message struct Result {
  output: float32;

  minOutput: float32;
  maxOutput: float32;
  assert(minOutput <= output);
  assert(output <= maxOutput);
}

transform verify(input: Measurement) -> Result {
  Result {
    output = 
      if ((input.minValue <= input.value) && 
          (input.value <= input.maxValue))
      then { input.value }
      else { input.defaultValue };
    minOutput = input.minValue;
    maxOutput = input.maxValue
  }
}

