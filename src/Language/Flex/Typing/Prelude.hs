module Language.Flex.Typing.Prelude where

import qualified Data.Map as Map
import Language.Flex.Syntax
import Language.Flex.Typing.TypingM

preludeTypingCtx :: TypingCtx
preludeTypingCtx =
  TypingCtx
    { _ctxTypes =
        Map.fromList
          [ let tyId = TypeId "string"
             in ( tyId,
                  CtxAlias
                    Alias
                      { aliasId = tyId,
                        aliasType = TypeArray TypeChar
                      }
                )
          ],
      _ctxFunctions = mempty,
      _ctxConstants = mempty,
      _ctxRefinedTypes = mempty,
      _ctxApplicants = mempty,
      _ctxCxparamNewtypeIds = mempty,
      _ctxCxparamIds = mempty
    }
