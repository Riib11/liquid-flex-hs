module Language.Flex.Refining.Prelude where

import qualified Language.Fixpoint.Types as F
import Language.Flex.Refining.Syntax (primitiveLocated)

-- ** Data Declarations (F.DataDecl)

preludeDataDecls :: [F.DataDecl]
preludeDataDecls =
  [ datadeclTuple
  ]

datadeclTuple :: F.DataDecl
datadeclTuple =
  F.DDecl
    { ddTyCon = tupleFTycon,
      ddVars = 2, -- how to use introduced ty vars?
      ddCtors =
        [ F.DCtor
            { dcName = primitiveLocated tupleTypeSymbol,
              dcFields =
                [ F.DField
                    { dfName = primitiveLocated tupleConstructorSymbol,
                      dfSort = error "TODO"
                    },
                  F.DField
                    { dfName = primitiveLocated tupleConstructorSymbol,
                      dfSort = error "TODO"
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

primitiveTycon :: F.Symbol -> F.FTycon
primitiveTycon = F.symbolFTycon . primitiveLocated
