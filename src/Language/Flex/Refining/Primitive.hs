{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# HLINT ignore "Use camelCase" #-}

module Language.Flex.Refining.Primitive where

import qualified Language.Fixpoint.Types as F
import Language.Flex.Refining.PrimitiveTH

$(makePrimitiveSymbols "tuple" "Tuple" ["Tuple"])
$(makePrimitiveSymbols "optional" "Optional" ["Some", "None"])
$(makePrimitiveSymbols "array" "Array" ["Nil", "Cons"])

primitiveDataDecls :: [F.DataDecl]
primitiveDataDecls =
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
                      { dfName = primitiveLocated $ F.symbol @String "value1",
                        dfSort = F.FVar 0
                      },
                    F.DField
                      { dfName = primitiveLocated $ F.symbol @String "value2",
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
                      { dfName = primitiveLocated $ F.symbol @String "value",
                        dfSort = F.FVar 0
                      }
                  ]
              }
          ]
      },
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
  ]

{-
tupleTypeLocatedSymbol :: F.Located F.Symbol
tupleTypeLocatedSymbol = primitiveLocated tupleTypeSymbol

tupleTypeSymbol :: F.Symbol
tupleTypeSymbol = "Tuple"

tupleConstructorSymbol :: F.Symbol
tupleConstructorSymbol = "makeTuple"

tupleFTycon :: F.FTycon
tupleFTycon = F.symbolFTycon tupleTypeLocatedSymbol

optionTypeSymbol :: F.Symbol
optionTypeSymbol = "Option"

optionConstructorSomeSymbol :: F.Symbol
optionConstructorSomeSymbol = "makeSome"

optionConstructorNoneSymbol :: F.Symbol
optionConstructorNoneSymbol = "makeNone"

arrayTypeSymbol :: F.Symbol
arrayTypeSymbol = "Array"

arrayConstructorNilSymbol :: F.Symbol
arrayConstructorNilSymbol = "makeNil"

arrayConstructorConsSymbol :: F.Symbol
arrayConstructorConsSymbol = "makeCons"
-}