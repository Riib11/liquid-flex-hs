I noticed that when trying to do refinement type checking, Liquid Fixpoint doesn't seem to be able to figure out how to instantiate existentials. For example here is a failed subtyping check:

```
unsafe:
  the term

    (EqualBits{b1 = (false : {VV : bit | VV == false}), b2 = (false : {VV : bit | VV == false})} : {VV : EqualBits | VV == EqualBits false false})
  
  was synthesized to sastisfy the refined type

    {VV : EqualBits | VV == EqualBits false false}
  
  but it was expected to satisfy the refined type

    {structTermStructure%0 : EqualBits | exists [b1 : bool; b2 : bool]   . structTermStructure%0 == EqualBits b1 b2}
```

where `EqualBits` is a struct that looks like

```
struct EqualBits {
  b1: bit;
  b2: bit;
  assert b1 == b2;
}
```

Is this a limitation with how existentials are handled in predicates, or am I just using it wrong?


```
VV == EqualBits false false ==> exists [b1 : bool; b2 : bool]  . VV == EqualBits b1 b2
```