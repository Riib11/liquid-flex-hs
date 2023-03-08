module Flex.Refining.Common where

import Flex.Flex

-- | Refining monad
type Refining a = FlexT a

runRefining :: Refining a -> FlexT a
runRefining = id
