module Flex.Refining where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Refining.CG (messageOfRefineError, unsafeCG)
import Flex.Refining.Check (reftPreterm, reftTerm)
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

  {-
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
  -}

  let tm =
        Term
          ( TermApp
              (AppVar "f")
              [ Term
                  ( TermApp
                      (AppVar "g")
                      -- [Term (TermLit (Syn.LiteralBit True)) ()]
                      [ let r =
                              unsafeCG $
                                reftTerm $
                                  Term
                                    (TermLiteral (Syn.LiteralBit False))
                                    (TypeAtomic mempty AtomicBit)
                         in Term
                              (TermLiteral (Syn.LiteralBit False))
                              (TypeAtomic r AtomicBit)
                      ]
                  )
                  (TypeAtomic mempty AtomicBit)
              ]
          )
          (TypeAtomic mempty AtomicBit)

  let ty = TypeAtomic mempty AtomicBit

  let env =
        foldr
          (uncurry extendEnv)
          emptyEnv
          [ ( -- f : bit{ b | b == true} -> bit
              "f",
              let r =
                    unsafeCG $
                      reftTerm
                        ( Term
                            (TermLiteral (Syn.LiteralBit True))
                            (TypeAtomic mempty AtomicBit)
                        )
               in TypeFunType $
                    FunType
                      [TypeAtomic r AtomicBit]
                      (TypeAtomic mempty AtomicBit)
            ),
            ( -- g : bit{ b : b == false } -> bit{ b : b == true }
              "g",
              let r1 =
                    unsafeCG $
                      reftTerm
                        ( Term
                            (TermLiteral (Syn.LiteralBit False))
                            (TypeAtomic mempty AtomicBit)
                        )
                  r2 =
                    unsafeCG $
                      reftTerm
                        ( Term
                            (TermLiteral (Syn.LiteralBit True))
                            (TypeAtomic mempty AtomicBit)
                        )
               in TypeFunType $
                    FunType
                      [TypeAtomic r1 AtomicBit]
                      (TypeAtomic r2 AtomicBit)
            )
          ]

  putStrLn $ "tm = " <> prettyShow tm
  putStrLn $ "ty = " <> prettyShow ty
  let query = unsafeCG $ genCheckQuery env tm ty
  result <- checkValid fp query
  exitWith =<< resultExitCode result
