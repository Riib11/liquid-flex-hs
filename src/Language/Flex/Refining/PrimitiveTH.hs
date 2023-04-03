{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Use ++" #-}

module Language.Flex.Refining.PrimitiveTH where

import Control.Monad
import Data.Functor
import qualified Language.Fixpoint.Types as F
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH

primitiveSourcePos :: F.SourcePos
primitiveSourcePos = F.dummyPos "<primitive>"

primitiveLocated :: a -> F.Located a
primitiveLocated val = F.Loc {loc = primitiveSourcePos, locE = primitiveSourcePos, val}

makePrimitiveSymbols :: String -> String -> [String] -> Q [Dec]
makePrimitiveSymbols label typeLabel ctorLabels = do
  fmap concat . sequence . concat $
    [ [ do
          let thisTypeSymbol_Name = mkName (label <> "_" <> typeLabel <> "TypeSymbol")
              thisTypeLocatedSymbol_Name = mkName (label <> "_" <> typeLabel <> "TypeLocatedSymbol")
              thisTypeFTycon_Name = mkName (label <> "_" <> typeLabel <> "FTycon")
          [d|
            $(varP thisTypeSymbol_Name) = F.symbol @String $(litE (StringL typeLabel))

            $(varP thisTypeLocatedSymbol_Name) = primitiveLocated $(varE thisTypeSymbol_Name)

            $(varP thisTypeFTycon_Name) = F.symbolFTycon $(varE thisTypeLocatedSymbol_Name)
            |]
      ],
      ctorLabels <&> \ctorLabel -> do
        let thisConstructorSymbol_Name = mkName (label <> "_" <> ctorLabel <> "ConstructorSymbol")
            thisConstructorLocatedSymbol_Name = mkName (label <> "_" <> ctorLabel <> "ConstructorLocatedSymbol")
        [d|
          $(varP thisConstructorSymbol_Name) = F.symbol @String $(litE (StringL ctorLabel))

          $(varP thisConstructorLocatedSymbol_Name) = primitiveLocated $(varE thisConstructorSymbol_Name)
          |]
    ]