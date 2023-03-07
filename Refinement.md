# Refinement

## Workflow

- parse
- type-check
- translate to refinement syntax
    - translate functions and constants, along with there annotated types
- refinement-check
    - for each declaration, check that the translated body satisfies the
      translated signature
    - this can involve referring to other refined structures in the body, in
      particular structure/newtype constructors
