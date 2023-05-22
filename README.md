# liquid-flex-hs

- [Notes](./Flex.md) on Flex
- [Notes](./Features.md) on planned/implemented features for Liquid Flex

## Compilation

You may need to run the following if on a M1 Mac.

```sh
C_INCLUDE_PATH="`xcrun --show-sdk-path`/usr/include/ffi" stack build
```

## Installation

Install the executable `liquid-flex-hs` with `stack`:

```sh
stack install
```

## Usage

You can run with `stack`, or install the executable and run that.

```sh
stack run -- <command> [--debug] [--verbose] [input]+
# or
liquid-flex-hs <command> [--debug] [--verbose]
```

The available commands are:

    - `help`: get some help
    - `parse`: parse an input module source file
    - `type`: type-check an input module source file (after parsing)
    - `refine`: refinement-check an input module source file (after parsing & type-checking)
