module Language.Flex.Refining.Prelude where

import Language.Flex.Refining.RefiningM

preludeRefiningCtx :: RefiningCtx
preludeRefiningCtx =
  RefiningCtx
    { _ctxStructures = mempty,
      _ctxVariants = mempty,
      _ctxFunctions = mempty,
      _ctxConstants = mempty
    }
