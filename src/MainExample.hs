module MainExample (main) where

import Flex.Flex
import Flex.Refining

main :: IO ()
-- main = Ex1.main
-- main = Refining.main
-- main = error "TODO: main"
main = do
  let tm = undefined
      ty = undefined
  (runFlexT topFlexEnv $ runRefining $ runCheck undefined tm ty)
    >>= ( \case
            Left fe -> undefined
            Right x0 -> undefined
        )