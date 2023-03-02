module Flex.Refining.Check where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Refining.CG
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
import System.Exit (exitWith)
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.Printf (printf)
import Utility

-- | Checking (check)
check :: Env -> Term -> BaseType -> CG Cstr
check env tm tyExp = do
  (cstr, tyInf) <- synth env tm
  cstr' <- checkSubtype tyInf tyExp
  return $ andCstrs [cstr, cstr']

checkBlock :: Env -> Block -> BaseType -> CG Cstr
checkBlock env ([], tm) tyExp = check env tm tyExp
checkBlock env (stmt : stmts, tm) tyExp = case stmt of
  StatementLet x mb_sigExp imp -> do
    (cstr, sigInf) <- synth env imp
    (cstr, sig) <-
      case mb_sigExp of
        -- if there is an annotated signature, then check that the
        -- implementation satisfies it i.e. the inferred signature is a subtype
        -- of the annotated (expected) signature
        Just sigExp -> checkSubtype sigInf sigExp <&> (,sigExp)
        -- otherwise, just use the inferred signature
        Nothing -> return (andCstr cstr trivialCstr, sigInf)
    -- universally quantify over the introduced value with the appropriate
    -- signature
    andCstr cstr . forallCstr x sig
      <$> checkBlock (extendEnv x (TypeBaseType sig) env) (stmts, tm) tyExp
  StatementAssert tm' ty' ->
    liftA2
      andCstr
      -- check that the term has the annotated type
      (check env tm' ty')
      (checkBlock env (stmts, tm) tyExp)

-- | Synthesizing (synth)
synth :: Env -> Term -> CG (Cstr, BaseType)
synth env (Term ptm ty) = case ptm of
  TermLit lit -> (trivialCstr,) <$> synthLiteral lit ty
  TermVar x -> (trivialCstr,) <$> synthCon env x
  TermBlock _ -> throwCG [RefineError "should never synthesize a TermBlock; should only ever check"]
  TermApp (ApplPrimFun pf) args -> synthAppPrimFun env pf args
  TermApp (ApplVar x) args -> do
    -- synth the arg types
    tyArgs <- (snd <$>) <$> (synth env `traverse` args)
    -- synth the fun type
    FunType params tyOut <- synthFun env x tyArgs
    -- check the args with their corresponding param types
    cstr <-
      andCstrs
        <$> mapM
          -- since function types are _simple_, don't need to update environment
          -- with values of arguments
          (\(tm, (_x, ty)) -> check env tm ty)
          (args `zip` params)
    -- since function types are _simple_, don't need to substitute parameters for
    -- their argument values in the output type
    return (cstr, tyOut)
  TermAscribe tm ty -> do
    cstr <- check env tm ty
    return (cstr, ty)

reftTerm :: Term -> CG F.Reft
reftTerm tm = F.exprReft <$> embedTerm tm

reftPreterm :: Preterm -> CG F.Reft
reftPreterm tm = F.exprReft <$> embedPreterm tm

synthAppPrimFun :: Env -> Syn.PrimFun -> [Term] -> CG (Cstr, BaseType)
synthAppPrimFun env Syn.PrimFunEq [tm1, tm2] = do
  -- synth type of first arg
  (cstr, ty1) <- synth env tm1
  -- check that second arg has same (unrefined) type
  cstr <- andCstr cstr <$> check env tm2 (unrefineBaseType ty1)
  r <- reftPreterm $ TermApp (ApplPrimFun Syn.PrimFunEq) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun env Syn.PrimFunAnd [tm1, tm2] = do
  cstr <- andCstrs <$> traverse (\tm -> check env tm (TypeAtomic F.trueReft AtomicBit)) [tm1, tm2]
  r <- reftPreterm $ TermApp (ApplPrimFun Syn.PrimFunAnd) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun env Syn.PrimFunOr [tm1, tm2] = do
  cstr <- andCstrs <$> traverse (\tm -> check env tm (TypeAtomic F.trueReft AtomicBit)) [tm1, tm2]
  r <- reftPreterm $ TermApp (ApplPrimFun Syn.PrimFunOr) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun env Syn.PrimFunNot [tm] = do
  cstr <- check env tm (TypeAtomic F.trueReft AtomicBit)
  r <- reftPreterm $ TermApp (ApplPrimFun Syn.PrimFunNot) [tm]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun _env pf args =
  throwCG
    [ RefineError $
        concat
          [ "primitive function",
            " '" <> show pf <> "' ",
            "applied to wrong number of arguments",
            " '" <> show args <> "'"
          ]
    ]

-- clears refinement from type
unrefineBaseType :: BaseType -> BaseType
unrefineBaseType = \case
  TypeAtomic _ atomic -> TypeAtomic mempty atomic

-- TODO: take `ty` into account
synthLiteral :: Literal -> BaseType -> CG BaseType
synthLiteral lit ty = do
  TypeAtomic <$> r <*> return atomic
  where
    atomic = case lit of
      Syn.LiteralInteger _ -> AtomicInt
      Syn.LiteralFloat _ -> AtomicFloat
      Syn.LiteralBit _ -> AtomicBit
      Syn.LiteralChar _ -> AtomicChar
      Syn.LiteralString _ -> AtomicString
    r = reftPreterm $ TermLit lit

synthCon :: Env -> F.Symbol -> CG BaseType
synthCon env x =
  lookupEnv x env
    >>= ( \case
            TypeBaseType ty -> return ty
            TypeFunType funTy ->
              throwCG
                [ RefineError . concat $
                    [ "type synthesis error;",
                      "expected the variable",
                      " '" <> show x <> "' ",
                      "to have be a constant, but it's actually a function of type",
                      " '" <> show funTy <> "'"
                    ]
                ]
        )

-- | synthFun
--
-- We need to know about `args` since some primitive functions are polymorphic.
synthFun :: Env -> F.Symbol -> [BaseType] -> CG FunType
-- types should be structurally the same (ignore refinement)
synthFun env x _args = case F.lookupSEnv x env of
  Just (TypeFunType funTy) -> return funTy
  Just _ -> throwCG [RefineError $ "expected to be a function id: " <> show x]
  Nothing -> throwCG [RefineError $ "unknown function id: " <> show x]

parseSymbol :: String -> F.Symbol
parseSymbol = FP.doParse' FP.lowerIdP "parseSymbol"

parsePred :: String -> F.Pred
parsePred = FP.doParse' FP.predP "parsePred"

-- | Subtype checking (checkSubtype)
--
-- Check that one type is a subtype of another type (taking refinements into
-- account).
checkSubtype :: BaseType -> BaseType -> CG Cstr
--    forall x : T, p x ==> (p' x')[x' := x]
--  ----------------------------------------------
--    {x : T | p x} <: {x' : T | p' y'}
checkSubtype ty1@(TypeAtomic r1 atom1) (TypeAtomic r2 atom2)
  | atom1 == atom2 =
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
  throwCG
    [ RefineError $
        "subtyping error; the type"
          <> (" '" <> show ty1 <> "' ")
          <> "cannot be checked to be a subtype of the type"
          <> (" '" <> show ty2 <> "' ")
    ]
