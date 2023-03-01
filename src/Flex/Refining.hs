module Flex.Refining where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
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
        TermApp
          (ApplPrimFun Syn.PrimFunOr)
          [ TermLit (Syn.LiteralBit True),
            TermLit (Syn.LiteralBit False)
          ]
  r <- case reftTerm (TermLit (Syn.LiteralBit True)) of
    Left errs -> error ("reftTerm error: " <> show errs)
    Right r -> return r
  let ty = TypeAtomic r AtomicBit

  putStrLn $ "tm = " <> show tm
  putStrLn $ "ty = " <> show ty
  res <- case genCheckQuery tm ty of
    Left errs ->
      pure $
        F.Crash
          (errs <&> \err -> (err, Just $ messageOfRefineError err))
          "genCheckQuery failure"
    Right query -> checkValid fp query
  exitWith =<< resultExitCode res

{-
Reflection

It's reasonable to define another version of the syntax that has refinements
attached to the types and terms in the appropriate places since the code that
deals with Flex purely syntactically without using Liquid Fixpoint should not
have to touch the Liquid Fixpoint-relevant data. I could in theory make the base
syntax polymorphic over the Liquid Fixpoint-relevant data, but that would
require a bunch of type variables in various places, and I don't even want to
reflect _everything_ from Flex, such as statements a-normal types, etc.
-}

-- | Type
--
-- Size constraints for numeric types are included in refinement info. In basic
-- Flex, function types have contextual parameters as well, but by the time we
-- are doing refinement-type checking, we already know that everything is
-- well-typed, so we can just have normal function types that have already
-- combined the arguments and contextual arguments into the appropriate list of
-- types.
type Type = Type_ F.Reft

data Type_ r
  = TypeBaseType (BaseType_ r)
  | TypeFunType (FunType_ r)
  deriving (Eq, Show)

-- | BaseType
type BaseType = BaseType_ F.Reft

-- TODO: handle more advanced types

-- | TypeTuple r ![Type_ r]
-- | TypeArray r !(Type_ r)
-- | TypeOptional r (Type_ r)
-- | TypeStructure Structure r
-- | TypeEnumerated Enumerated r
-- | TypeVariant Variant r
-- | TypeNewtype NewType_ r
data BaseType_ r
  = TypeAtomic r Atomic
  deriving
    (Eq, Show, Functor)

data Atomic
  = AtomicInt
  | AtomicUInt
  | AtomicFloat
  | AtomicBit
  | AtomicChar
  | AtomicString
  deriving (Eq, Show)

-- | FunType
--
-- Liquid Flex's function types are simple in that the they cannot express
-- dependency of one type's refinement on a preceeding parameter's value.
type FunType = FunType_ F.Reft

data FunType_ r
  = FunType ![(F.Symbol, BaseType_ r)] !(BaseType_ r)
  deriving (Eq, Show)

-- | Subable (Subtypeable)
instance F.Subable r => F.Subable (Type_ r) where
  syms = \case
    TypeBaseType baseTy -> F.syms baseTy
    TypeFunType funTy -> F.syms funTy

  substa f = \case
    TypeBaseType baseTy -> TypeBaseType $ F.substa f baseTy
    TypeFunType funTy -> TypeFunType $ F.substa f funTy
  substf f = \case
    TypeBaseType baseTy -> TypeBaseType $ F.substf f baseTy
    TypeFunType funTy -> TypeFunType $ F.substf f funTy
  subst f = \case
    TypeBaseType baseTy -> TypeBaseType $ F.subst f baseTy
    TypeFunType funTy -> TypeFunType $ F.subst f funTy

instance F.Subable r => F.Subable (BaseType_ r) where
  syms = \case
    TypeAtomic r _ -> F.syms r
  substa f = \case
    TypeAtomic r atomic -> TypeAtomic (F.substa f r) atomic
  substf f = \case
    TypeAtomic r atomic -> TypeAtomic (F.substf f r) atomic
  subst f = \case
    TypeAtomic r atomic -> TypeAtomic (F.subst f r) atomic

instance F.Subable r => F.Subable (FunType_ r) where
  syms (FunType params outTy) =
    concatMap
      (F.syms . snd)
      params
      <> F.syms outTy
  substa f (FunType params outTy) =
    FunType
      (second (F.substa f) <$> params)
      (F.substa f outTy)
  substf f (FunType params outTy) =
    FunType
      (second (F.substf f) <$> params)
      (F.substf f outTy)
  subst f (FunType params outTy) =
    FunType
      (second (F.subst f) <$> params)
      (F.subst f outTy)

-- | Substitution
--
-- substitute `x` for `y` in `thing` via Subable
subst :: F.Subable a => a -> F.Symbol -> F.Symbol -> a
subst thing x y = F.subst (F.mkSubst [(x, F.expr y)]) thing

-- -- TODO: do i need this? could only work on Term(Var|Literal)
-- subst' :: F.Subable a => a -> F.Symbol -> Term -> a
-- subst' thing x y = F.subst sigma thing
--   where
--     sigma = F.mkSubst [(x, embedVar y)]

-- | Embedding
--
-- Embed a term as a LF expression
embedTerm :: Term -> CG F.Expr
embedTerm = \case
  TermLit lit -> return $ embedLiteral lit
  TermVar x -> return $ embedVar x
  TermBlock block -> error "TODO: embedTerm TermBlock"
  TermApp (ApplPrimFun Syn.PrimFunEq) [tm1, tm2] ->
    F.PAtom F.Eq <$> embedTerm tm1 <*> embedTerm tm2
  TermApp (ApplPrimFun Syn.PrimFunAnd) [tm1, tm2] ->
    F.PAnd <$> traverse embedTerm [tm1, tm2]
  TermApp (ApplPrimFun Syn.PrimFunOr) [tm1, tm2] ->
    F.POr <$> traverse embedTerm [tm1, tm2]
  TermApp (ApplPrimFun Syn.PrimFunNot) [tm] ->
    F.PNot <$> embedTerm tm
  TermApp (ApplPrimFun pf) args -> throwCG [RefineError $ "invalid primitive function application: " <> show (TermApp (ApplPrimFun pf) args)]
  -- TODO: folds in the right direction??
  TermApp (ApplVar x) args -> foldM (\e tm -> F.EApp e <$> embedTerm tm) (embedVar x) args
  TermAscribe tm _ty -> embedTerm tm

embedVar :: F.Symbol -> F.Expr
embedVar = F.expr

embedLiteral :: Literal -> F.Expr
embedLiteral = \case
  Syn.LiteralInteger n -> F.expr n
  Syn.LiteralFloat x -> error "TODO: how to embed Float?"
  Syn.LiteralBit b -> if b then F.PTrue else F.PFalse
  Syn.LiteralChar c -> F.expr (pack [c])
  Syn.LiteralString txt -> F.expr txt

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
type Cstr = H.Cstr RefineError

trivialCstr :: Cstr
trivialCstr = H.CAnd []

andCstr :: Cstr -> Cstr -> Cstr
andCstr cstr1 cstr2 = andCstrs [cstr1, cstr2]

andCstrs :: [Cstr] -> Cstr
andCstrs = H.CAnd

forallCstr :: F.Symbol -> BaseType -> Cstr -> Cstr
forallCstr x ty cstr = case sortPred x ty of
  Just (srt, prd) -> H.All (H.Bind x srt prd (RefineError "forallCstr")) cstr
  _ -> cstr

-- subtyping constraint (??)
headCstr :: F.Expr -> Cstr
headCstr e = H.Head (H.Reft e) (RefineError $ "Subtype error: " <> show e)

reftSymbol :: F.Reft -> F.Symbol
reftSymbol (F.Reft (x, _)) = x

reftExpr :: F.Reft -> F.Expr
reftExpr (F.Reft (_, e)) = e

sortPred :: F.Symbol -> BaseType -> Maybe (F.Sort, H.Pred)
sortPred x = \case
  TypeAtomic r atom ->
    Just
      ( case atom of
          AtomicInt -> F.intSort
          AtomicUInt -> F.intSort
          AtomicFloat -> F.realSort
          AtomicBit -> F.boolSort
          AtomicChar -> F.charSort
          AtomicString -> F.strSort,
        mkReft r
      )
  where
    mkReft r = H.Reft (subst (reftExpr r) (reftSymbol r) x)

-- | Term
--
-- TODO: desc
--
-- TODO: tmp disable advanced terms
-- | TermTuple ![Term]
-- | TermArray ![Term]
-- | TermTuple ![Term]
-- | TermStructure Structure (Map.Map Text Term)
-- | TermMember Type -- Term Text
-- | TermConstructor Type
-- | TermMatch Type
data Term
  = TermLit !Literal
  | TermVar !F.Symbol
  | TermBlock !Block
  | TermApp !Appl [Term]
  | TermAscribe !Term !BaseType
  deriving (Eq, Show)

data Appl
  = ApplPrimFun Syn.PrimFun
  | ApplVar F.Symbol
  deriving (Eq, Show)

type Block = ([Statement], Term)

data Statement
  = StatementLet !F.Symbol !(Maybe BaseType) !Term
  | StatementAssert !Term !BaseType
  deriving (Eq, Show)

-- | Label
type Label = F.SrcSpan

class HasLabel a where
  getLabel :: a -> Label

instance HasLabel Term where
  getLabel _ = F.dummySpan

-- | Constraint Generation monad
type CG a = Either [RefineError] a

throwCG :: [RefineError] -> CG a
throwCG = Left

-- | RefineError
newtype RefineError = RefineError String
  deriving (Generic, Show)

instance NFData RefineError

instance Exception [RefineError]

messageOfRefineError :: RefineError -> String
messageOfRefineError (RefineError msg) = msg

labelOfRefineError :: RefineError -> Label
labelOfRefineError _ = F.dummySpan

instance F.PPrint RefineError where
  pprintTidy k = F.pprintTidy k . refineErrorFP

instance F.Fixpoint RefineError where
  toFix = PJ.text . messageOfRefineError

instance F.Loc RefineError where
  srcSpan = labelOfRefineError

fpRefineError :: F.Error1 -> RefineError
fpRefineError e = RefineError (show $ F.errMsg e)

refineErrorFP :: RefineError -> F.Error
refineErrorFP err =
  F.err
    (labelOfRefineError err)
    (PJ.text $ messageOfRefineError err)

renderRefineError :: RefineError -> IO PJ.Doc
renderRefineError (RefineError msg) = do
  -- TODO: can also look up snippet where error originated
  return $ PJ.text msg

renderRefineErrors :: [RefineError] -> IO PJ.Doc
renderRefineErrors errs = do
  errs' <- mapM renderRefineError errs
  return $ PJ.vcat (PJ.text "Errors found!" : PJ.text "" : errs')

-- | Env
type Env = F.SEnv Type

emptyEnv :: Env
emptyEnv = F.emptySEnv

extendEnv :: F.Symbol -> Type -> Env -> Env
extendEnv = F.insertSEnv

lookupEnv :: F.Symbol -> Env -> CG Type
lookupEnv x env = case F.lookupSEnv x env of
  Nothing ->
    throwCG
      [ RefineError $
          "Can't find variable's refinement type in environment: "
            <> show x
      ]
  Just ty -> return ty

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
synth _env (TermLit lit) = (trivialCstr,) <$> synthLiteral lit
synth env (TermVar x) = (trivialCstr,) <$> synthCon env x
synth _env (TermBlock _) = throwCG [RefineError "should never synthesize a TermBlock; should only ever check"]
synth env (TermApp (ApplPrimFun pf) args) = synthAppPrimFun env pf args
synth env (TermApp (ApplVar x) args) = do
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
synth env (TermAscribe tm ty) = do
  cstr <- check env tm ty
  return (cstr, ty)

reftTerm :: Term -> CG F.Reft
reftTerm tm = F.exprReft <$> embedTerm tm

synthAppPrimFun :: Env -> Syn.PrimFun -> [Term] -> CG (Cstr, BaseType)
synthAppPrimFun env Syn.PrimFunEq [tm1, tm2] = do
  -- synth type of first arg
  (cstr, ty1) <- synth env tm1
  -- check that second arg has same (unrefined) type
  cstr <- andCstr cstr <$> check env tm2 (unrefineBaseType ty1)
  r <- reftTerm $ TermApp (ApplPrimFun Syn.PrimFunEq) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun env Syn.PrimFunAnd [tm1, tm2] = do
  cstr <- andCstrs <$> traverse (\tm -> check env tm (TypeAtomic F.trueReft AtomicBit)) [tm1, tm2]
  r <- reftTerm $ TermApp (ApplPrimFun Syn.PrimFunAnd) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun env Syn.PrimFunOr [tm1, tm2] = do
  cstr <- andCstrs <$> traverse (\tm -> check env tm (TypeAtomic F.trueReft AtomicBit)) [tm1, tm2]
  r <- reftTerm $ TermApp (ApplPrimFun Syn.PrimFunOr) [tm1, tm2]
  return (cstr, TypeAtomic r AtomicBit)
synthAppPrimFun env Syn.PrimFunNot [tm] = do
  cstr <- check env tm (TypeAtomic F.trueReft AtomicBit)
  r <- reftTerm $ TermApp (ApplPrimFun Syn.PrimFunNot) [tm]
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

synthLiteral :: Literal -> CG BaseType
synthLiteral lit = do
  TypeAtomic <$> r <*> return atomic
  where
    atomic = case lit of
      Syn.LiteralInteger _ -> AtomicInt
      Syn.LiteralFloat _ -> AtomicFloat
      Syn.LiteralBit _ -> AtomicBit
      Syn.LiteralChar _ -> AtomicChar
      Syn.LiteralString _ -> AtomicString
    r = reftTerm $ TermLit lit

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

-- | Query
--
-- The query includes all of the constraints gathered up during
-- checking/synthesizing.
type Query = H.Query RefineError

-- Generates a query that checks that the term has the type.
genCheckQuery :: Term -> BaseType -> CG Query
genCheckQuery tm ty =
  H.Query [] []
    <$> check emptyEnv tm ty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty

-- | Result
type Result = F.FixResult RefineError

resultExitCode :: Result -> IO ExitCode
resultExitCode res = do
  F.colorStrLn (F.colorResult res) (resultString res)
  case resultErrors res of
    [] -> return ()
    errs -> putStrLn . PJ.render =<< renderRefineErrors errs
  return (F.resultExit res)

resultErrors :: Result -> [RefineError]
resultErrors = \case
  -- TODO: why does each err have a Maybe String also?
  F.Crash errs msg -> RefineError ("Crash: " <> msg) : (fst <$> errs)
  F.Unsafe _ errs -> errs
  F.Safe {} -> []

resultString :: Result -> String
resultString = \case
  F.Crash _ msg -> "Crash!: " ++ msg
  F.Unsafe {} -> "Unsafe"
  F.Safe {} -> "Safe"

-- | checkValid
checkValid :: FilePath -> Query -> IO Result
checkValid fp = checkValidWithConfig fp fpConfig

fpConfig :: FC.Config
fpConfig =
  FC.defConfig
    { FC.eliminate = FC.Some
    }

checkValidWithConfig :: FilePath -> FC.Config -> Query -> IO Result
checkValidWithConfig fp config query = do
  dumpQuery fp query
  fmap snd . F.resStatus <$> HS.solve config query

dumpQuery :: FilePath -> Query -> IO ()
dumpQuery fp query = when True do
  let smtFile = Files.extFileName Files.Smt2 fp
  Misc.ensurePath smtFile
  writeFile smtFile (PJ.render . F.pprint $ query)