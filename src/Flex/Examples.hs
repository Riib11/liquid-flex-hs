{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Flex.Examples
  ( module_Simple,
  )
where

import Flex.Shallow
import Flex.Syntax

module_Simple :: IO Module
module_Simple =
  module_
    "SimpleFunctions"
    []
    [ readDeclaration "fun f0() -> bit { true }",
      readDeclaration "fun f1(x: bit) -> bit { true }",
      readDeclaration "const b: bit = true"
    ]
