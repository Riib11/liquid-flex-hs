module Result where

// A result type as specified by some external component's interface.

message struct Result {
  status: Status;
  value: Optional<int32>;
  assert(
    ((status == Status#Stable)   && (value != None)) ||
    ((status == Status#Volatile) && (value != None)) ||
    ((status == Status#Error)    && (value == None))
  );
}

enum Status uint4 {
  Stable = 0;
  Volatile = 1;
  Error = 2;
}

transform incrementResult(result: Result) -> Result {
  if (result.status == Status#Error) { result } else {
    // Stabilize before using value
    let stableResult = stabilize(result);

    match(stableResult.value) {
      // stableResult must be stable, and so must have Some value, and so this
      // is an impossible case.
      None => assertFalse;
      Some(x) => Result { status = Status#Stable; value = Some(x + 1) }
    }
  
  }
}

// Reset a volatile value to 0
// Only works on Stable or Volatile Results
// Note: Can't make this a transform
function stabilize(result: Result) -> Result {
  match (result.status) {
    Status#Stable => result;
    Status#Volatile => Result { status = Status#Stable; value = Some(0) };
    Status#Error => assertFalse
  }
}