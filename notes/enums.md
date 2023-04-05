# Enums

- can specify underlying bitvector encoding
- there _should_ be a way of converting enum value to is underlying value
  - maybe use `cast`?
    - can cast totally from enum to value
    - can cast partially from value to enum
- _maybe_ can think of enum as a refined newtype
  - downside: lose info about which label an instance came from
  - can convert enum matches to if-then-elses
  - leads to an intuitive way to think about totality
  - enums can often be big (which could be bad if you are encoding it as a disjunction)

```
  enum Coin bit {
      Heads = true;
      Tails = false;
  }

<=>

  newtype Coin {
    b : bit;
    assert(b == true || b == false)
  }

  if c.b == true  then ... else
  if c.b == false then ...
  else assert(false)

<=>

  match c with {
    Coin#Heads => ...;
    Coin#Tails => ...;
  }
```