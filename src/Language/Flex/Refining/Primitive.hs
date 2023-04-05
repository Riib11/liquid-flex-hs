{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# HLINT ignore "Use camelCase" #-}

module Language.Flex.Refining.Primitive where

import qualified Language.Fixpoint.Types as F
import Language.Flex.Refining.PrimitiveTH

$(makePrimitiveSymbols "tuple" "Tuple" ["Tuple"])
$(makePrimitiveSymbols "optional" "Optional" ["Some", "None"])

-- $(makePrimitiveSymbols "array" "Array" ["Nil", "Cons"])

primitiveDataDecls :: [F.DataDecl]
primitiveDataDecls =
  F.muSort -- !TODO does this take the fixpoint of recursive types or something?
    [ -- data Tuple a b
      F.DDecl
        { ddTyCon = tuple_TupleFTycon,
          ddVars = 2,
          ddCtors =
            [ -- Tuple : (value1 : a) -> (value2 : b) -> Tuple a b
              F.DCtor
                { dcName = tuple_TupleConstructorLocatedSymbol,
                  dcFields =
                    [ F.DField
                        { dfName = primitiveLocated $ F.symbol @String "tupleField1",
                          dfSort = F.FVar 0
                        },
                      F.DField
                        { dfName = primitiveLocated $ F.symbol @String "tupleField2",
                          dfSort = F.FVar 1
                        }
                    ]
                }
            ]
        },
      -- data Optional a
      F.DDecl
        { ddTyCon = optional_OptionalFTycon,
          ddVars = 1,
          ddCtors =
            [ -- None : Optional a
              F.DCtor
                { dcName = optional_NoneConstructorLocatedSymbol,
                  dcFields = []
                },
              -- Some : (a : value) -> Optional a
              F.DCtor
                { dcName = optional_SomeConstructorLocatedSymbol,
                  dcFields =
                    [ F.DField
                        { dfName = primitiveLocated $ F.symbol @String "OptionalSomeField1",
                          dfSort = F.FVar 0
                        }
                    ]
                }
            ]
        }
        {-
        -- !TODO there is already a sort called "Array", so just use that rather
        -- than re-inventing arrays
        ,
        -- data Array a
        F.DDecl
          { ddTyCon = array_ArrayFTycon,
            ddVars = 1,
            ddCtors =
              [ -- Nil : Array a
                F.DCtor
                  { dcName = array_NilConstructorLocatedSymbol,
                    dcFields = []
                  },
                -- Cons : (head : a) (tail : Array a) -> Array a
                F.DCtor
                  { dcName = array_ConsConstructorLocatedSymbol,
                    dcFields =
                      [ F.DField
                          { dfName = primitiveLocated $ F.symbol @String "head",
                            dfSort = F.FVar 0
                          },
                        F.DField
                          { dfName = primitiveLocated $ F.symbol @String "tail",
                            dfSort = F.fAppTC array_ArrayFTycon [F.FVar 0]
                          }
                      ]
                  }
              ]
          }
        -}
    ]

{-
primitiveRecRewrites :: [F.Rewrite]
primitiveRecRewrites =
  [ -- recTuple : (a -> b -> x) -> Tuple<a, b> -> x
    -- recTuple f (Tuple a b) = f a b
    -- F.Equ
    --   { eqName = tuple_TupleRecSymbol,
    --     eqArgs = [("f", F.FFunc (F.FVar 0) (F.FVar 1)), ("t", _)],
    --     eqBody = _wz5,
    --     eqSort = _wz6,
    --     eqRec = False
    --   }
    F.SMeasure
      { smName = _,
        smDC = _, -- !TODO data constructor?
        smArgs = _,
        smBody = _
      }
  ]
-}
