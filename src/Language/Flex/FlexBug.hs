module Language.Flex.FlexBug where

import Language.Flex.FlexM (FlexLog (FlexLog))

throw :: FlexLog -> a
throw (FlexLog title body) =
  error $
    unlines
      [ replicate 40 '=',
        "[flex bug in " <> title <> "]",
        body,
        replicate 40 '='
      ]
