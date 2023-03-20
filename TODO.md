# TODO

To add a new type to the refinement syntax:
- Flex.Refining.Syntax
    - add constructor to data Type_ r
- Flex.Refining.Translating
    - make output of a case in transType
    - handle case of sortOfType
- Flex.Refining.Constraint
    - handle case of sortPred

- [ ] Structures
    - [ ] constructing, destructing (with refinement)
- [ ] Newtypes
    - [ ] destructing (with refinement)
- [ ] Enums
    - [ ] pattern matching
- [ ] Variants
    - [ ] pattern matching
- [ ] Proper freshening of variables when inlining functions
- [ ] expand refinement test suite
- [ ] totality
    - [x] assertions
    - [ ] exhaustive branching
    - [ ] casting
        - there's a predicate for each casting type pair
    - [ ] try
        - reasoning in refinements: make optional result explicit, and try works
          over that?

To get refinement type checking working with at least base types (e.g. just
`int*` types):
- [ ] some primitive functions in Flex
    - [x] add, div
        - these are special because they require figuring out the type based on
          the types of the arguments
    - make a test that shows div passing/failing since it has the non-zero denom
      refinement
- [ ] syntax translation: Flex -> Liquid Flex
    - [ ] handle Structures and Newtypes
    - [ ] pattern matching
        - [ ] variants, enums
    - [x] handle function implicit parameters
- [ ] examples/tests for translation
    - [x] literals
    - [x] simple applications (no implicit parameters)
- [ ] setup golden file test suite style



- syntax changes:
    - enum and variant constructors are always prefixed by their enum/variant
      name, and variant constructors always require parentheses (even if has no
      params)