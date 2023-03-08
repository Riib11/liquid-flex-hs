module Flex.Refining
  ( module Flex.Refining.Common,
    module Flex.Refining.Check,
    module Flex.Refining.Query,
    module Flex.Refining.Syntax,
    module Flex.Refining.Translation,
    checkModule,
    checkDeclaration,
    checkFunction,
    checkConstant,
    runCheck,
  )
where

import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Flex
import Flex.Refining.Check
import Flex.Refining.Common
import Flex.Refining.Query
import Flex.Refining.Syntax
import Flex.Refining.Translation
import qualified Flex.Syntax as Base
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

checkModule :: Env -> Base.Module -> Refining ()
checkModule = undefined

checkDeclaration :: Env -> Base.Declaration -> Refining ()
checkDeclaration = undefined

checkFunction :: Env -> Base.Function -> Refining ()
checkFunction = undefined

checkConstant :: Env -> Base.Constant -> Refining ()
checkConstant env con = do
  k <- case Base.constantBody con of
    Base.DefinitionBodyTerm tm -> return \f -> f tm
    Base.DefinitionBodyDerived (Just tm) -> return \f -> f tm
    _ -> return \_f -> return ()
  k \tm -> do
    tm' <- transTerm tm
    runCheck env tm' (termType tm')

runCheck :: Env -> Term -> BaseType -> Refining ()
runCheck env tm ty = do
  query <- genCheckQuery env tm ty
  result <- liftIO $ checkValid "TODO: filepath" query
  case result of
    -- TODO: print better
    F.Crash x s -> throwError . refineError $ "Crash: " <> show (x, s)
    F.Unsafe st res -> throwError . refineError $ "Unsafe: " <> show (st, res)
    F.Safe st -> return ()

{-
main :: IO ()
main = do
  let fp :: FilePath
      fp = "Refining.hs"

  -- -- EXAMPLE: this is Unsafe because `1 == 2` is not equal to `true`
  -- let tm =
  --       TermApp
  --         (ApplPrimFun Base.PrimFunEq)
  --         [ TermLit (Base.LiteralInteger 1),
  --           TermLit (Base.LiteralInteger 2)
  --         ]
  -- r <- case reftTerm (TermLit (Base.LiteralBit True)) of
  --   Left errs -> error ("reftTerm error: " <> show errs)
  --   Right r -> return r
  -- let ty = TypeAtomic r AtomicBit

  -- -- EXAMPLE: this is Safe because `true || false` is equal to `true`
  -- let tm =
  --       Term
  --         ( TermApp
  --             (AppPrimFun Base.PrimFunOr)
  --             [ Term
  --                 (TermLit (Base.LiteralBit True))
  --                 (TypeAtomic mempty AtomicBit),
  --               Term
  --                 (TermLit (Base.LiteralBit False))
  --                 (TypeAtomic mempty AtomicBit)
  --             ]
  --         )
  --         (TypeAtomic mempty AtomicBit)
  --     rtm =
  --       Term
  --         (TermLit (Base.LiteralBit True))
  --         (TypeAtomic mempty AtomicBit)
  -- r <- case reftTerm rtm of
  --   Left errs -> error ("reftTerm error: " <> show errs)
  --   Right r -> return r
  -- let ty = TypeAtomic r AtomicBit

  let tm =
        Term
          ( TermApp
              (AppVar "f")
              [ Term
                  ( TermApp
                      (AppVar "g")
                      -- [Term (TermLit (Base.LiteralBit True)) ()]
                      [ let r =
                              unsafeRefining $
                                reftTerm $
                                  Term
                                    (TermLiteral (Base.LiteralBit False))
                                    (TypeAtomic mempty AtomicBit)
     unsafeRefining                    in Term
                              (TermLiteral (Base.LiteralBit False))
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
                    unsafeRefining $
                      reftTerm
                        ( Term
                            (TermLiteral (Base.LiteralBit True))
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
                    unsafeRefining $
                      reftTerm
                        ( Term
                            (TermLiteral (Base.LiteralBit False))
                            (TypeAtomic mempty AtomicBit)
                        )
                  r2 =
                    unsafeRefining $
                      reftTerm
                        ( Term
                            (TermLiteral (Base.LiteralBit True))
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
  let query = unsafeRefining $ genCheckQuery env tm ty
  result <- checkValid fp query
  exitWith =<< resultExitCode result
-}