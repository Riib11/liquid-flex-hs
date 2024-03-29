# Flex

- Namespaces:
  - modules
  - types
  - terms
- Names cannot be shadowed (with respect to their corresponding namespace)

## Declaration

- Declarations are only at the top-level of a module, and are defined in
  mutually-recursive style i.e. the order of the declarations doesn't matter
  i.e. all declarations are added to the context at the same time.

### Structure

- list of fields with associated types
- can have refinement
- can be a _message_
- multiple refinements are conjoined together for refinement-checking

Example:
```
message struct Bounded {
  x: int32;
  min: int32;
  max: int32;
  assert(min <= x);
  assert(x <= max);
}
```

### Newtype

- one field with associated type
- can have refinement
- contextual parameters must be newtypes

Example:
```
newtype PositiveInt32 {
  x: int32;
  assert(0 <= x);
}

constant p: PositiveInt32 = PositiveInt32(10)
```

### Variant

- list of constructors, each with associated list of param types
- no params is the same as empty list of params
- constructing a variant term requires referencing the variant type e.g.
  `Nat.Zero()` or `Nat.Suc(1)`, where the `()` is required even if the
  constructor has no params (which is the same as empty list of params)

Example:
```
variant Nat {
  Zero;
  Suc(n: Nat);
}

constant myZero: Nat = Nat.Zero
```

### Enumerated Type

- associated literal type
- list of constructors, with associated literal value
- constructing an enum term requires referencing the enum type e.g. `Day.Monday`

Example
```
enum Day string {
  Monday = "Monday";
  Tuesday = "Tuesday";
  ...
}

constant today: Day = Day.Monday
```

### Type Alias

- alias for a type

Example:
```
type Result = Optional<int32>
```

### Type Defaulting

Some types can be defaulted if not concretized during type-checking. For
example, in the following, the type of `x` is defaulted to `int32`.
```
function f() -> bit {
  let x = 1;
  assert(x == x);
  true
}
```

### Function

(note about my terminology: a _parameter_ is what a function is abstractly
defined over; an _argument_ is a concrete value that is passed in a specific
application of a function)

- list of params with associated types
- list of contextual params with associated newtypes (must be newtypes!)
  - the associated type of each contextual param must be unique among the
    contextual params of the function
  - contextual args can be given explicitly via `f(<args>) giving (<cxargs>)` 
  - when given explicitly, the contextul args can be given in any order; which
    contextual param each contextual arg corresponds to is determined uniquely
    by its type
  - when given implicitly, expects there to only be one named term in context
    that has the expected type, and that term is given as the contextual
    argument
  - contextual args can only be implicitly inferred from among the in-scope
    contextual arguments (of the function whose body you're currently in)
- output type
- can be a _transform_
- if is a _transform_:
  - param types and output type must be _message_ types
  - !TODO should contextual param types also have to be _message_ types?
  - function definition is refinement-checked
- if is not a _transform_:
  - during refinement-checking, function calls are inlined
  - function definition is NOT refinement-checked

Example
```
function add1(x: int32) -> float32 {
  let y = x + 1;
  y
}

function mod(x: int32) given (?modulus: int32) {
  x % ?modulus
}

function foo() given (?x: int32, ?flag: Bool) {
  if ?flag then {
    ?x
  } else {
    -?x
  }
}

transform function doubleBoundedint32(bounded: Boundedint32) -> Boundedint32 =
  Boundedint32 {
    x = 2 * bounded.x;
    min = 2 * bounded.min;
    max = 2 * bounded.max;
  }

function main() -> int32 {
  let modulus = 2;
  let flag = true;

  let _ = add1(0);
  // next 2 equivalent
  let _ = mod(1); // implicitly finds that `modulus` is only named term in context that has type `int32`
  let _ = mod(1) given (modulus);
  
  let _ = foo(); // finds `modulus` and `flag` to be the implicit args
  // next 2 equivalent
  let _ = foo() given (1, true);
  let _ = foo() given (true, 1);

  let bounded = Boundedint32 { x = 2; min = 0; max = 4 };
  let _ = doubleBoundedint32(bounded);

  0
}
```

### Constant

- must be type-annotated, since at top level.

Example
```
const x: int32 = 10
const b: Bool = true
```

## Term

During type-checking, terms are incrementally annotated with their types. After
type-checking is finished, each term is annotated with its normalized type, and
primitive polymorphic functions are annotated with their monomorphized type.

### Literal

- for the sake of type-checking, are implicitly wrapped in a `cast`

### Cast

- `cast` is a primitive function
- `cast<a, b> : (a: a) -> b` where `a` is castable to `b`
- casting is partial
- "`a` is castable to `b`" relation:
  - `uint[m] or int[n] is castable to uint[m'] or int[n']`
  - `float[m] is castable to float[n]`
- casts are unwrapped during typechecking

### Primitive Polymorphism

- Flex does not support user-defined polymorphism, but it does contain some
  primitive polymorphism
- each instance of primitive polymorphism is monomorphized during type-checking
- examples:
  - constructors of parametric types
    - `none<a> -> Optional<a>`
    - `some<a>(a: a) -> Optional<a>`
    - `[a, ...]<a> -> Array<a>`
    - `(a, b, c)<a, b, c> -> Tuple<a, b, c>`
  - `try<a>(a: a) -> Optional<a>`
  - `cast<a, b>(a: a) -> b` where `a` is castable to `b`

### Try

- In Flex, partiality is implicit by default, but can be made explicit via `try`
- During type-checking, `try` is not treated specially
- During refinement-checking, `try`'s value is opaque, so it's not very useful
  to refer to it in refinements
- During runtime, `try` will evaluate its argument, and if any exception is raised
  then the `try` evaluates to `None`, and otherwise results in `Some(v)` where
  `v` is the value of the argument

### Blocks

- A block is a (possibly non-empty) list of statements followed by a term
- `let` statement
  - introduces a new term name into context for the result of the block
  - can use pattern, which raises exception if fails match
- `assert` statement
  - durign refinement-checking, adds assertion's refinement to current
    constraints
  - during runtime, raises exception if refinement is false

## Pattern

`let` statements and `case` expressions use patterns

```
<pat> ::=
  | <term-id> // introduce new name
  | <pat> : <ty> // ascribe
  | <pat> extends <type-id> // structure extends
  | <constr>?[(*[<pat>])] // variant/newtype/enum constructor
  | !TODO other patterns
```

