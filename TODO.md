# TODO


To get refinement type checking working with at least base types (e.g. just `int*` types):
- [ ] some primitive functions in Flex
    - [x] add, div
        - these are special because they require figuring out the type based on the types of the arguments
    - make a test that shows div passing/failing since it has the non-zero denom
      refinement
- [ ] syntax translation: Flex -> Liquid Flex
    - [ ] handle Structures and Newtypes
    - [ ] handle function implicit parameters
- [ ] examples/tests for translation
    - [x] literals
    - [x] simple applications (no implicit parameters)