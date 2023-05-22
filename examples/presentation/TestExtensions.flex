module TestExtensions where

struct Test {
  successes: uint32;
  failures: uint32;
}

// uninterpreted predicate: isTest

struct TestSnapshot extends Test {
  ongoing: uint32;
  assert(1 <= ongoing);
}

// uninterpreted predicate: isTestSnapshot
// axiom: forall x. isTestSnapshot(x) ==> 1 <= x.ongoing && isTest(x)

struct TestSnapshotWithKnownTotal extends TestSnapshot {
  total: uint32;
  assert(total == successes + failures + ongoing)
}

// uninterpreted predicate: TestSnapshotWithKnownTotal
// axiom: forall x . isTestSnapshotWithKnownTotal(x) ==> 
//          x.total == x.successes + x.failures + x.ongoing + isTestSnapshot(x)

function progressPercent(test: TestSnapshotWithKnownTotal) -> int32 {
  // asserts test.total != 0
  test.successes + test.failures / test.total
}

