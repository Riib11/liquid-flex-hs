module MainExample (main) where

import Flex.Flex
import Flex.Refining

main :: IO ()
main = do
  let tm = undefined
      ty = undefined
  (runFlexM topFlexEnv . runRefining $ checkValid =<< makeCheckTermQuery tm ty)
    >>= ( \case
            Left fe -> undefined
            Right x0 -> undefined
        )