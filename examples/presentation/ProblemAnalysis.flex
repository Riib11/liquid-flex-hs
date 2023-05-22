module ProblemAnalysis where

// Problem

message struct Problem {
  kind: ProblemKind;
  msg: string;
  subproblem: Optional<Problem>
}

enum ProblemKind {
  Suggestion;
  Warning;
  Error;
}

// CriticalProblem

message struct CriticalProblem {
  problem: Problem;
  assert isCriticalProblemKind(problem.kind);
}

function isCriticalProblemKind(kind: ProblemKind) -> bit {
  kind == Warning ||
  kind == Error
}

// Analysis

message struct Analysis {
  criticalProblem: Optional<CriticalProblem>;
}

// analyzeProblem

transform analyzeProblem(problem: Problem) -> Analysis {
  if (isCriticalProblemKind(problem.kind)) {
    Analysis { criticalProblem = Some(problem) }
  } else {
      match(problem.subproblem) {
        None => Analysis { criticalProblem = None };
        Some(subproblem) => findCriticalProblem(subproblem)
      }
  }
}

