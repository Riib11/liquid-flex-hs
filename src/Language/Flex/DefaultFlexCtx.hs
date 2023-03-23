module Language.Flex.DefaultFlexCtx where

import Language.Flex.FlexM (FlexCtx (..))

defaultFlexCtx :: FlexCtx
defaultFlexCtx =
  FlexCtx
    { flexVerbose = False,
      flexSourceFilePath = "<empty>"
    }

-- -- for debugging
-- defaultFlexCtx :: FlexCtx
-- defaultFlexCtx =
--   FlexCtx
--     { flexVerbose = True,
--       flexSourceFilePath = "<empty>"
--     }