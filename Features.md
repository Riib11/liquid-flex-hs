# Liquid Flex Features

- polymorphic optionals
    - supports refinement reasoning under optional
- polymorphic tuples
    - supports refinement reasoning under tuples
- enums
    - has specified representation type
    - each case has specified representation literal value
    - are translated to variants during refinement
- variants
    - can be recursive
    - [ ] supports refinement reasoning about value of match
- structures
    - can be recursive
    - can have extensions
        - inherits fields
        - inherits refinements
        - are elaborated during typing
- type aliases
    - cannot be recursive
    - are elaborated during typing
- newtypes
    - translated to structures during refinement
- try, throw
    - [ ] refinement reasoning about value of try
- functions
    - can have _implicit arguments_ which are uniquely specified by their types, which must be newtypes
    - inlined (including assertions) during refining
    - are NOT refined on their own
- transforms
    - can have _implicit arguments_ which are uniquely specified by their types, which must be newtypes
    - refined according to types
    - refinements specified by the input types of a transform definition are _assumed_ when refining that transform
    - refinements specified by the output type of a transform call are _assumed_ when refining the context in which that transform call appears
- constant
    - supports refinement reasoning about value during refining
- literals
    - during typing, are treated as implicitly wrapped in a `cast`
- casts
    - `cast` is a primitive function
    - total casts are unwapped during typing
    - partial casts yield obligations during refining
- blocks
    - a block is a list of statements followed by a result term
    - a `let` statement locally binds a value in the block, which can be reasoned about during refining
    - an `assert` statement asserts a bit-typed term to be true, which yields obligations during refining
- matches
    - a match expression supports pattern-matching on optionals, variants, and enums
    - supports refinement reasoning about conditions under each branch
- if
    - supports refinement reasoning about conditions under each branch