module Language.Flex.Refining.Prelude where

import qualified Language.Fixpoint.Types as F

tupleTypeSym :: F.Symbol
tupleTypeSym = "Tuple"

tupleCtorSym :: F.Symbol
tupleCtorSym = "makeTuple"

optionTypeSym :: F.Symbol
optionTypeSym = "Option"

optionCtorSomeSym :: F.Symbol
optionCtorSomeSym = "makeSome"

optionCtorNoneSym :: F.Symbol
optionCtorNoneSym = "makeNone"

arrayTypeSym :: F.Symbol
arrayTypeSym = "Array"

arrayCtorNilSym :: F.Symbol
arrayCtorNilSym = "makeNil"

arrayCtorConsSym :: F.Symbol
arrayCtorConsSym = "makeCons"
