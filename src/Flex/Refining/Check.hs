{-# HLINT ignore "Redundant return" #-}
module Flex.Refining.Check where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when, zipWithM)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Bifunctor (second)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Flex (debug, refineError)
import Flex.Refining.Common
import Flex.Refining.Constraint
import Flex.Refining.Embedding
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

-- | Checking
--
-- The type needs to be re-synthesized even though the term is already annotated
-- because it could result in constraints and aslo the annotated type is
-- unrefined.
checkTerm :: Term -> BaseType -> Refining Cstr
checkTerm tm tyExp = do
  (cstr, tyInf) <- synthTerm tm
  debug $ "checkTerm: tyExp = " <> prettyShow tyExp
  debug $ "checkTerm: tyInf = " <> prettyShow tyInf
  -- TODO: do i actually need to check that the expected type is satisfialbe?
  -- No, right? since that will just be propogated upwards and ultimately result
  -- in a refinement failure.
  --
  -- cstr <- checkSubtype tyExp (const F.trueReft <$> tyExp) <&> andCstr cstr
  cstr <- checkSubtype tyInf tyExp <&> andCstr cstr
  return cstr

checkBlock :: Block -> BaseType -> Refining Cstr
checkBlock ([], tm) tyExp = checkTerm tm tyExp
checkBlock (stmt : stmts, tm) tyExp = case stmt of
  StatementLet x imp -> do
    (cstr, sig) <- synthTerm imp
    -- universally quantify over the introduced value with the appropriate
    -- signature
    andCstr cstr . forallCstr x sig
      <$> extendEnv x (TypeBaseType sig) (checkBlock (stmts, tm) tyExp)
  StatementAssert tm' ->
    liftA2
      andCstr
      -- checkTerm that the term has the `true` refinement type
      (checkTerm tm' trueBaseType)
      (checkBlock (stmts, tm) tyExp)

-- | Synthesizing (synth)
synthTerm :: Term -> Refining (Cstr, BaseType)
synthTerm (Term ptm ty) = case ptm of
  TermLiteral lit -> (trivialCstr,) <$> synthLiteral lit ty
  TermVar x -> (trivialCstr,) <$> synthCon x
  TermBlock _ -> throwError . refineError $ "should never synthesize a TermBlock; should only ever check"
  TermApp (AppPrimFun pf) args -> synthAppPrimFun ty pf args
  TermApp (AppVar x) args -> do
    -- synthTerm the arg types
    tyArgs <- (snd <$>) <$> (synthTerm `traverse` args)
    -- synthTerm the fun type
    FunType params tyOut <- synthFun x tyArgs
    -- checkTerm the args with their corresponding param types; note that since
    -- function types are _simple_, don't need to update environment with values
    -- of arguments
    cstr <- andCstrs <$> zipWithM checkTerm args params
    -- since function types are _simple_, don't need to substitute parameters
    -- for their argument values in the output type
    return (cstr, tyOut)

reftTerm :: Term -> Refining F.Reft
reftTerm tm = F.exprReft <$> embedTerm tm

reftPreterm :: Preterm -> Refining F.Reft
reftPreterm tm = F.exprReft <$> embedPreterm tm

synthAppPrimFun :: BaseType -> Syn.PrimFun -> [Term] -> Refining (Cstr, BaseType)
synthAppPrimFun ty Syn.PrimFunEq [tm1, tm2] = do
  -- synthTerm type of first arg
  (cstr, ty1) <- synthTerm tm1
  -- check that second arg has same (unrefined) type
  cstr <- andCstr cstr <$> checkTerm tm2 (unrefineBaseType ty1)
  r <- reftPreterm $ TermApp (AppPrimFun Syn.PrimFunEq) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun ty Syn.PrimFunAnd [tm1, tm2] = do
  cstr <- andCstrs <$> traverse (\tm -> checkTerm tm (TypeAtomic F.trueReft AtomicBit)) [tm1, tm2]
  r <- reftPreterm $ TermApp (AppPrimFun Syn.PrimFunAnd) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun ty Syn.PrimFunOr [tm1, tm2] = do
  cstr <- andCstrs <$> traverse (\tm -> checkTerm tm (TypeAtomic F.trueReft AtomicBit)) [tm1, tm2]
  r <- reftPreterm $ TermApp (AppPrimFun Syn.PrimFunOr) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun ty Syn.PrimFunNot [tm] = do
  cstr <- checkTerm tm (TypeAtomic F.trueReft AtomicBit)
  r <- reftPreterm $ TermApp (AppPrimFun Syn.PrimFunNot) [tm]
  return (cstr, TypeAtomic r AtomicBit)
-- TODO: may need to extract the numeric types into `TypeNumber :: NumberSize ->
-- Type` so that I can keep track of the modulus to use in these primitive
-- numeric operations; for example to keep `+` total, need to wrap around the
-- numeric size, but currently I don't actually have that info statically
-- because I only store it in the refinement; so i probably want AtomicInt to
-- have a NumericSize field
synthAppPrimFun ty Syn.PrimFunPlus [tm1, tm2] = do
  let r = baseTypeReft ty
  cstr <- andCstrs <$> traverse (\tm -> checkTerm tm (TypeAtomic r AtomicInt)) [tm1, tm2]
  r' <- reftPreterm $ TermApp (AppPrimFun Syn.PrimFunPlus) [tm1, tm2]
  return (cstr, TypeAtomic r' AtomicInt)
synthAppPrimFun ty Syn.PrimFunDiv [tm1, tm2] = do
  let r = baseTypeReft ty
  cstr <- checkTerm tm1 (TypeAtomic r AtomicInt)
  -- the refinement {tm2 /= 0}
  pred_nonzero_tm2 <-
    embedPreterm $
      TermApp (AppPrimFun Syn.PrimFunNot) . List.singleton $
        Term
          ( TermApp
              (AppPrimFun Syn.PrimFunEq)
              [ tm2,
                Term (TermLiteral (Syn.LiteralInteger 0)) (bitBaseType mempty)
              ]
          )
          (bitBaseType mempty)
  -- in addition to checking that tm2 satisfies the numeric type bounds, also
  -- check that it's nonzero
  cstr <-
    checkTerm
      tm2
      ( TypeAtomic
          ( F.reft
              (F.reftBind r)
              (F.reftPred r F.&.& pred_nonzero_tm2)
          )
          AtomicInt
      )
      <&> andCstr cstr
  r' <- reftPreterm $ TermApp (AppPrimFun Syn.PrimFunDiv) [tm1, tm2]
  -- TODO: do i need to include the result that `a/b != 0`? or will z3 be smart
  -- enough to figure that out on its own
  return (cstr, TypeAtomic r' AtomicInt)
synthAppPrimFun _ty pf args =
  throwError . refineError $
    concat
      [ "primitive function",
        " '" <> show pf <> "' ",
        "applied to wrong number of arguments",
        " '" <> prettyShow args <> "'"
      ]

-- clears refinement from type
unrefineBaseType :: BaseType -> BaseType
unrefineBaseType = \case
  TypeAtomic _ atomic -> TypeAtomic mempty atomic

-- TODO: take `ty` into account
synthLiteral :: Literal -> BaseType -> Refining BaseType
synthLiteral lit ty = do
  TypeAtomic <$> r <*> return atomic
  where
    atomic = case lit of
      Syn.LiteralInteger _ -> AtomicInt
      Syn.LiteralFloat _ -> AtomicFloat
      Syn.LiteralBit _ -> AtomicBit
      Syn.LiteralChar _ -> AtomicChar
      Syn.LiteralString _ -> AtomicString
    r = reftPreterm $ TermLiteral lit

synthCon :: F.Symbol -> Refining BaseType
synthCon x =
  lookupEnv x
    >>= ( \case
            TypeBaseType ty -> return ty
            TypeFunType funTy ->
              throwError . refineError . concat $
                [ "type synthesis error;",
                  "expected the variable",
                  " '" <> show x <> "' ",
                  "to have be a constant, but it's actually a function of type",
                  " '" <> show funTy <> "'"
                ]
        )

-- | synthFun
--
-- We need to know about `args` since some primitive functions are polymorphic.
synthFun :: F.Symbol -> [BaseType] -> Refining FunType
-- types should be structurally the same (ignore refinement)
synthFun x _args =
  lookupEnv x >>= \case
    (TypeFunType funTy) -> return funTy
    _ -> throwError . refineError $ "expected to be a function id: " <> show x

-- | Subtype checking (checkSubtype)
--
-- Check that one type is a subtype of another type (taking refinements into
-- account).
checkSubtype :: BaseType -> BaseType -> Refining Cstr
--    forall x : T, p x ==> (p' x')[x' := x]
--  ----------------------------------------------
--    {x : T | p x} <: {x' : T | p' y'}
checkSubtype ty1@(TypeAtomic r1 atom1) ty2@(TypeAtomic r2 atom2)
  | atom1 == atom2 = do
      debug $ "checkSubtype"
      debug $ "  " <> prettyShow ty1
      debug $ "  <: "
      debug $ "  " <> prettyShow ty2
      return $
        forallCstr
          (reftSymbol r1)
          ty1
          ( headCstr $
              subst
                (reftExpr r2)
                (reftSymbol r2)
                (reftSymbol r1)
          )
checkSubtype ty1 ty2 =
  throwError . refineError $
    "subtyping error; the type"
      <> (" '" <> prettyShow ty1 <> "' ")
      <> "cannot be checked to be a subtype of the type"
      <> (" '" <> prettyShow ty2 <> "' ")
