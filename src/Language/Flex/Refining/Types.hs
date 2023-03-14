module Language.Flex.Refining.Types where

import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Types as F
import Language.Flex.Refining.RefiningM

-- | Query
--
-- The query includes all of the constraints gathered up during
-- checking/synthesizing.
type Query = H.Query RefiningError

-- | Result
--
-- Result received back after submitting query
type Result = F.FixResult RefiningError

-- | Constraints
--
-- In Liquid Fixpoint, `H.Cstr` has the following form:
--
--    data Cstr a
--      = Head  !Pred !a                  -- ^ p
--      | CAnd  ![Cstr a]                 -- ^ c1 /\ ... /\ cn
--      | All   !(Bind a)  !(Cstr a)      -- ^ \all x:t. p => c
--      | Any   !(Bind a)  !(Cstr a)      -- ^ \exi x:t. p /\ c or is it \exi x:t. p => c?
--      deriving (Data, Typeable, Generic, Functor, Eq)
type Cstr = H.Cstr RefiningError
