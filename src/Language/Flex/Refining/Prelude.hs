module Language.Flex.Refining.Prelude where

import qualified Language.Fixpoint.Types as F
import Language.Flex.Refining.Syntax (primitiveLocated)

-- ** Data Declarations (F.DataDecl)

preludeDataDecls :: [F.DataDecl]
preludeDataDecls =
  [ datadeclTuple
  ]

-- TODO: is it actually proper to use F.FVar to refer to the type variables?
datadeclTuple :: F.DataDecl
datadeclTuple =
  F.DDecl
    { ddTyCon = tupleFTycon,
      ddVars = 2,
      ddCtors =
        [ F.DCtor
            { dcName = primitiveLocated tupleConstructorSymbol,
              dcFields =
                [ F.DField
                    { dfName = primitiveLocated tupleField1Symbol,
                      dfSort = F.FVar 0
                    },
                  F.DField
                    { dfName = primitiveLocated tupleField2Symbol,
                      dfSort = F.FVar 1
                    }
                ]
            }
        ]
    }

-- ** Type Constructors (F.Tycon)

tupleFTycon :: F.FTycon
tupleFTycon = primitiveTycon "Tuple"

tupleTypeSymbol :: F.Symbol
tupleTypeSymbol = "Tuple"

tupleConstructorSymbol :: F.Symbol
tupleConstructorSymbol = "MakeTuple"

tupleField1Symbol :: F.Symbol
tupleField1Symbol = "TupleField1"

tupleField2Symbol :: F.Symbol
tupleField2Symbol = "TupleField2"

primitiveTycon :: F.Symbol -> F.FTycon
primitiveTycon = F.symbolFTycon . primitiveLocated
