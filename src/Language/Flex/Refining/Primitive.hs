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