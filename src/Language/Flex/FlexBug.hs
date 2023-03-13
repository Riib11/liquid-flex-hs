module Language.Flex.FlexBug where

import Language.Flex.FlexM (FlexLog (..))
import Text.PrettyPrint.HughesPJ
  ( braces,
    render,
    text,
    vcat,
    (<+>),
  )

throw :: FlexLog -> a
throw (FlexLog {..}) =
  error . render $
    vcat
      [ text $ replicate 40 '=',
        braces $ "bug in" <+> logLabel,
        logBody,
        text $ replicate 40 '='
      ]
