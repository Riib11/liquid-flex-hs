module Flex.Refining.Common where

import Flex.Flex

-- | Refining monad
type Refining a = FlexM a

runRefining :: Refining a -> FlexM a
runRefining = id
