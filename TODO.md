# TODO

Refinements that are implied by types:
- refine via type:
    - inputs to a transform
    - transform application
    - cast


Limitations of prototype:
- structure fields must have unique names (because of how translated to Liquid
  Fixpoints datatype interface)

- [ ] assume refinements on parameters of transforms and outputs of transform
  calls
- [ ] reflecting `match`
- [x] fix named type normalization
- [x] Enums
    - [x] translate to Variant
- [x] Newtypes
    - [x] translate to Structure
- [x] Variants
    - [x] translate type
        - `V ~~> { VV: V | exists x y . VV = V1 x || VV == V2 y }
        - [x] DONT use expr-exists
    - [x] constructing (straightforward reflection)
        - `V1 a : { VV: V | VV = V1 a }`
    - [x] pattern matching
- [x] Function inlining
    - [x] implement
    - [x] bug: renaming of function params is only applied to syntactic term and
          not to embedded term
- [x] If-then-else
    - [x] implemented
    - [x] introduce local assumption
    - [x] needs more tests
- [ ] totality
    - [x] assertions
    - [ ] exhaustive branching
    - [ ] try
        - reasoning in refinements: make optional result explicit, and try works
          over that?
- [ ] Primitives (includes reflection)
    - [x] `==`
        - [ ] `!=`
    - [x] `&&`, `||`, `!`
        [ ] `==>`
    - [x] `+`
        - [ ] `-`, `*`, `/`, `mod`, `<=`, `<`, `>`, `>=`, ...
- [x] Structures
    - [x] constructing
        - `S{ x = 1; y = 2 } : { VV: S | VV = S 1 2 }`
    - [x] field access
        - `s.x: { VV | exists y . s == S VV y }`
- [x] Proper freshening of variables when inlining functions
- [x] expand refinement test suite
- [x] note that can introduce equations into query context (Query.qEqns) but
  then also have to add them to the constants (H.qCon) 
    - could do this instead of what i do now for `let` and `function` inlining
    - but, could this have scoping issues? might be best to just leave
      everything in refinements where i know it works

To get refinement type checking working with at least base types (e.g. just
`int*` types):

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