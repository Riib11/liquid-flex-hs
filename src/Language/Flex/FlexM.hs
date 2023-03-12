module Language.Flex.FlexM where

import Control.Lens
import Control.Monad.Writer (WriterT)

-- | The `FlexM` monad is the base monad under which all the impure comptuations
-- are done in this implementation. In particular, all state is
-- handled by `FlexM`.
type FlexM = WriterT [FlexLog] IO

data FlexLog
  = FlexLog
      String
      -- ^ title
      String
      -- ^ body

throwFlexError :: FlexLog -> a
throwFlexError (FlexLog title body) =
  error $
    unlines
      [ replicate 40 '=',
        "[flex error in " <> title <> "]",
        body,
        replicate 40 '='
      ]

throwFlexBug :: FlexLog -> a
throwFlexBug (FlexLog title body) =
  error $
    unlines
      [ replicate 40 '=',
        "[flex bug in " <> title <> "]",
        body,
        replicate 40 '='
      ]
