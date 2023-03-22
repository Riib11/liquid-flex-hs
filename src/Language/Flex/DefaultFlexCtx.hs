module Language.Flex.DefaultFlexCtx where

import Language.Flex.FlexM (FlexCtx (..))

defaultFlexCtx :: FlexCtx
defaultFlexCtx =
  FlexCtx
    { flexVerbose = False,
      sourceFilePath = "<empty>"
    }

-- -- for debugging
-- defaultFlexCtx :: FlexCtx
-- defaultFlexCtx =
--   FlexCtx
--     { flexVerbose = True,
--       sourceFilePath = "<empty>"
--     }