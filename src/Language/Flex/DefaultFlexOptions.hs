module Language.Flex.DefaultFlexOptions where

import Language.Flex.FlexM (FlexOptions (..))

defaultFlexOptions :: FlexOptions
defaultFlexOptions =
  FlexOptions
    { flexVerbose = False,
      sourceFilePath = "<empty>"
    }

-- -- for debugging
-- defaultFlexOptions :: FlexOptions
-- defaultFlexOptions =
--   FlexOptions
--     { flexVerbose = True,
--       sourceFilePath = "<empty>"
--     }