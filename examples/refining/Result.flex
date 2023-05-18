module Result where

// A result type as specified by some external component's interface.

message Result {
  status: Status;
  value: Optional<int32>;
  assert(
    ((status == Status#Ok) && (value != None)) ||
    ((status == Status#Unstable) && (value != None)) ||
    ((status == Status#Error) && (value == None))
  );
}

enum Status string {
  Ok = "Ok";
  Unstable = "Unstable";
  Error = "Error";
}

// Incrementing a result:
//   - incrementing an Ok result yields an Ok result with an incremented value
//   - incrementing a Unstable result yields an Error result (discard the value)
//   - incrementing an Error result yields an Error
transform incrementResult(result: Result) -> Result {
  match result.value with {
    None => result;
    Some(x) => {
      match result.status with {
        Status#Ok => Result{ value = Some(x + 1); status = Status#Ok };
        Status#Unstable => Result{ value = None; status = Status#Error };
        Status#Error => {
          // this branch is impossible
          assert(false);
          result
        };
      }
    };
  }
}