module Language.Flex.FlexBug where

import Language.Flex.FlexM (FlexLog (..))
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint.HughesPJ (braces, brackets, render, text, vcat, ($$), (<+>))

throw :: FlexLog -> a
throw (FlexLog {..}) =
  error . render $
    text (replicate 40 '=')
      $$ brackets (text "bug:" <+> logLabel)
      $$ logBody
      $$ text (replicate 40 '=')
