module Language.Flex.FlexBug where

import Control.Lens
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (gets)
import Language.Flex.FlexM
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass
import Prelude hiding ((<>))

-- prints the message and trace
throw :: MonadFlex m => Doc -> m a
throw doc = liftFlex do
  trace <- gets (^. flexTrace)
  stack <- asks (^. flexStack)
  throwError
    FlexLog
      { logMark = stack,
        logBody =
          vcat
            [ "[ bug: message ]" <> text (replicate 40 '='),
              doc,
              "[ bug: trace ]" <> text (replicate 40 '='),
              vcat $ fmap ("[>]" <+>) $ pPrint <$> trace
            ]
      }
