module Flex.Refining where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Refining.CG (messageOfRefineError)
import Flex.Refining.Check (reftTerm)
import Flex.Refining.Query (checkValid, genCheckQuery, resultExitCode)
import Flex.Refining.Syntax
import Flex.Syntax (Id, Literal, ModuleId)
import qualified Flex.Syntax as Syn
import GHC.Generics
import GHC.IO.Exception (ExitCode)
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as F
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Parse as FP
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import PrettyShow (PrettyShow (prettyShow))
import System.Exit (exitWith)
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.Printf (printf)
import Utility

main :: IO ()
main = do
  let fp :: FilePath
      fp = "Refining.hs"

  {-
  -- EXAMPLE: this is Unsafe because `1 == 2` is not equal to `true`
  let tm =
        TermApp
          (ApplPrimFun Syn.PrimFunEq)
          [ TermLit (Syn.LiteralInteger 1),
            TermLit (Syn.LiteralInteger 2)
          ]
  r <- case reftTerm (TermLit (Syn.LiteralBit True)) of
    Left errs -> error ("reftTerm error: " <> show errs)
    Right r -> return r
  let ty = TypeAtomic r AtomicBit
  -}

  -- EXAMPLE: this is Safe because `true || false` is equal to `true`
  let tm =
        Term
          ( TermApp
              (AppPrimFun Syn.PrimFunOr)
              [ Term
                  (TermLit (Syn.LiteralBit True))
                  (TypeAtomic mempty AtomicBit),
                Term
                  (TermLit (Syn.LiteralBit False))
                  (TypeAtomic mempty AtomicBit)
              ]
          )
          (TypeAtomic mempty AtomicBit)
      rtm =
        Term
          (TermLit (Syn.LiteralBit True))
          (TypeAtomic mempty AtomicBit)
  r <- case reftTerm rtm of
    Left errs -> error ("reftTerm error: " <> show errs)
    Right r -> return r
  let ty = TypeAtomic r AtomicBit

  putStrLn $ "tm = " <> prettyShow tm
  putStrLn $ "ty = " <> prettyShow ty
  res <- case genCheckQuery tm ty of
    Left errs ->
      pure $
        F.Crash
          (errs <&> \err -> (err, Just $ messageOfRefineError err))
          "genCheckQuery failure"
    Right query -> checkValid fp query
  exitWith =<< resultExitCode res
