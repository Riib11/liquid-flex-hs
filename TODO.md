# TODO

- [ ] TermField
    - `s.xI: { VV | exists a1 ... aN . s == S a1 ... aN && VV == aI }`

- [ ] note that can introduce equations into query context (Query.qEqns) but
  then also have to add them to the constantst (H.qCon) 
    - could do this instead of what i do now for `let` and `function` inlining
    - but, could this have scoping issues? might be best to just leave
      everything in refinements where i know it works
- [ ] field of strcuture 

- [x] Structures
    - [ ] constructing, destructing (with refinement)
- [ ] Newtypes
    - [ ] destructing (with refinement)
    - [ ] can be translated as Structures?
- [ ] Enums
    - [ ] pattern matching
- [ ] Variants
    - [ ] pattern matching
- [x] Proper freshening of variables when inlining functions
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


- [x] where does SPRITE add and use data types in the `Env`? it seems that my
  issue is that it doesn't know about the primitive type constructors that I am
  introducing, and thinks they are free
  - [x] so it doesn't seem like SPRITE actually does introduce those type
    constructor variables into context
  - [x] the type variable environment is initialized as empty in fact
  - [x] so that implies that i dont think my problem is that hte type constructor is not in context


To add a new type to the refinement syntax:
- Flex.Refining.Syntax
    - add constructor to data Type
    - handle case of Pretty Type 
- Flex.Refining.Embedding
    - if can be constructed by primitive, then yield result in embedPrimitive
    - handle case of sortOfType
    - if involves a new type constructor, write a tyCon<type-constructor-name>
- Flex.Refining.Translating
    - make output of a case in transType
    - handle case of sortOfType
- Flex.Refining.Constraint
    - handle case of sortPred
- Flex.Refining.Check
    - if can be produced by primitive, add as result of synthPrimitive



- syntax changes:
    - enum and variant constructors are always prefixed by their enum/variant
      name, and variant constructors always require parentheses (even if has no
      params)