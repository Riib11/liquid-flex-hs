module Example.Example1 where

import Control.DeepSeq
import Control.Exception
import Control.Monad (void, when)
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import GHC.Generics
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as F
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Parse as FP
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import System.Exit
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.Printf (printf)

type Label = F.SrcSpan

-- | Syntax

-- | Type
data Type r
  = IntType r
  | FunType ![(F.Symbol, Type r)] !(Type r)
  deriving (Show)

type ReftType = Type F.Reft

-- | Term
type Term = Term' Label

data Term' l
  = ImmTerm !(Imm' l)
  | AppTerm !(Imm' l) ![Imm' l] l
  | DefFunTerm !(Bind' l) !ReftType !(Term' l) !(Term' l) l
  | DefConTerm !(Bind' l) !(Term' l) !(Term' l) l
  | AnnTerm !(Term' l) !ReftType l
  deriving (Functor, Show)

-- | Bind
type Bind = Bind' Label

data Bind' l = Bind !F.Symbol l
  deriving (Eq, Ord, Functor, Show)

bindId :: Bind' l -> F.Symbol
bindId (Bind x _) = x

-- | Decl
type Decl = Decl' Label

data Decl' l = Decl (Bind' l) (Term' l) l
  deriving (Functor, Show)

-- | Imm
type Imm = Imm' Label

data Imm' l
  = VarImm !F.Symbol l
  | PrimImm !Prim l
  deriving (Functor, Show)

-- | Prim
data Prim
  = IntPrim Integer
  | AddPrim
  | SubPrim
  deriving (Eq, Ord, Show)

primType :: Prim -> ReftType
primType (IntPrim n) = IntType (F.exprReft (F.expr n))
primType AddPrim =
  FunType
    [(x, IntType mempty), (y, IntType mempty)]
    (IntType (F.Reft (parseSymbol "z", parsePred "x + y")))
  where
    x = parseSymbol "x"
    y = parseSymbol "y"
primType SubPrim =
  FunType
    [(x, IntType mempty), (y, IntType mempty)]
    (IntType (F.Reft (parseSymbol "z", parsePred "x - y")))
  where
    x = parseSymbol "x"
    y = parseSymbol "y"

-- | Subable
instance F.Subable r => F.Subable (Type r) where
  syms = \case
    IntType r -> F.syms r
    FunType params rty -> concatMap (\(_, rty') -> F.syms rty') params <> F.syms rty
  substa f = \case
    IntType r -> IntType (F.substa f r)
    FunType params rty -> FunType (second (F.substa f) <$> params) (F.substa f rty)
  substf f = \case
    IntType r -> IntType (F.substf f r)
    FunType params rty -> FunType (second (F.substf f) <$> params) (F.substf f rty)
  subst f = \case
    IntType r -> IntType (F.subst f r)
    FunType params rty -> FunType (second (F.subst f) <$> params) (F.subst f rty)

-- | GetLabel
class HasLabel t where
  getLabel :: t l -> l

instance HasLabel Term' where
  getLabel (ImmTerm imm) = getLabel imm
  getLabel (AppTerm _ _ l) = l
  getLabel (DefConTerm _ _ _ l) = l
  getLabel (DefFunTerm _ _ _ _ l) = l
  getLabel (AnnTerm _ _ l) = l

instance HasLabel Bind' where
  getLabel (Bind _ l) = l

instance HasLabel Decl' where
  getLabel (Decl _ _ l) = l

instance HasLabel Imm' where
  getLabel (VarImm _ l) = l
  getLabel (PrimImm _ l) = l

-- | Substitution
subst :: F.Subable a => a -> F.Symbol -> F.Symbol -> a
subst thing x y = substImm thing x (VarImm y ())

substImm :: F.Subable a => a -> F.Symbol -> Imm' l -> a
substImm thing x y = F.subst su thing
  where
    su = F.mkSubst [(x, immExpr y)]

immExpr :: Imm' l -> F.Expr
immExpr = \case
  VarImm x _ -> F.expr x
  PrimImm (IntPrim n) _ -> F.expr n
  _imm -> error "`immExp` should never be called with this kind of Imm"

-- | Cstr
type Cstr = H.Cstr UserError -- constraint

trivialCstr :: Cstr
trivialCstr = H.CAnd []

andCstr :: Cstr -> Cstr -> Cstr
andCstr cstr1 cstr2 = andCstrs [cstr1, cstr2]

andCstrs :: [Cstr] -> Cstr
andCstrs = H.CAnd

forallCstr :: Label -> F.Symbol -> ReftType -> Cstr -> Cstr
forallCstr l x rty cstr = case sortPred x rty of
  Just (so, p) -> H.All (H.Bind x so p (UserError "forallCstr" l)) cstr
  _ -> cstr

-- subtyping constraint (??)
headCstr :: Label -> F.Expr -> Cstr
headCstr l e = H.Head (H.Reft e) (UserError "Subtype error" l)

sortPred :: F.Symbol -> ReftType -> Maybe (F.Sort, H.Pred)
sortPred x = \case
  IntType (F.Reft (v, p)) -> Just (F.intSort, H.Reft (subst p v x))
  _ -> Nothing

-- | Parse
parseSymbol :: String -> F.Symbol
parseSymbol = FP.doParse' FP.lowerIdP "parseSymbol"

parsePred :: String -> F.Pred
parsePred = FP.doParse' FP.predP "parsePred"

-- | Env
type Env = F.SEnv ReftType

emptyEnv :: Env
emptyEnv = F.emptySEnv

extendEnv :: Bind -> ReftType -> Env -> Env
extendEnv (Bind x _) = F.insertSEnv x

lookupEnv :: F.Symbol -> Label -> Env -> Refining ReftType
lookupEnv x l env = case F.lookupSEnv x env of
  Nothing -> throwRefining [UserError (pack $ "Can't find variable's refinement type in environment: " <> show x) l]
  Just rty -> return rty

-- | Refining
type Refining a = Either [UserError] a

throwRefining :: [UserError] -> Refining a
throwRefining = Left

-- | Check/Synth
-- Check handles DefConTerm, DefFunTerm
-- Synth handles ImmTerm, AppTerm, AnnTerm
check :: Env -> Term -> ReftType -> Refining Cstr
check env (DefConTerm bx@(Bind x lx) imp bod _) ty = do
  (cstr, sig) <- synth env imp
  cstr' <- check (extendEnv bx sig env) bod ty
  return $ andCstrs [cstr, forallCstr lx x sig cstr']
check env (DefFunTerm _ _ _ _ _) _ = error "unimplemented: check DefFunTerm"
check env tm tyExp = do
  (cstr, tyInf) <- synth env tm
  cstr' <- subType (getLabel tm) tyInf tyExp
  return (andCstrs [cstr, cstr'])

checkImm :: Env -> Imm -> ReftType -> Refining Cstr
checkImm env imm rty = do
  rty' <- synthImm env imm
  subType (getLabel imm) rty' rty

synth :: Env -> Term -> Refining (Cstr, ReftType)
synth env = \case
  ImmTerm imm -> (trivialCstr,) <$> synthImm env imm
  AppTerm imm args l ->
    synthImm env imm >>= \case
      FunType params rty -> do
        cstrs <-
          mapM
            (\(arg, (_x, rty')) -> checkImm env arg rty')
            (args `zip` params)
        return
          ( andCstrs cstrs,
            foldr
              (\((x, _xrty), arg) rty' -> substImm rty' x arg)
              rty
              (params `zip` args)
          )
      _ -> throwRefining [UserError "Applicant does not have function type" l]
  AnnTerm tm rty _ -> (,rty) <$> check env tm rty
  tm -> throwRefining [UserError (pack $ "`synth` should never be called on this form: " <> show tm) (getLabel tm)]

synthImm :: Env -> Imm -> Refining ReftType
synthImm env = \case
  VarImm x l -> lookupEnv x l env
  PrimImm pr _ -> return $ primType pr

-- check that `rty` is a subtype of `rty'`
subType :: Label -> ReftType -> ReftType -> Refining Cstr
subType l rty@(IntType (F.Reft (v, _))) (IntType (F.Reft (v', q'))) =
  return $ forallCstr l v rty (headCstr l (subst q' v' v))
subType l (FunType params1 rtyOut1) (FunType params2 rtyOut2) = do
  -- check that the expected input types are subtypes of the actual input types
  cstrIn <-
    foldr (\cstr1 cstr2 -> andCstrs [cstr1, cstr2]) trivialCstr
      <$> mapM
        ( \((x1, rtyIn1), (x2, rtyIn2)) ->
            subType l rtyIn2 rtyIn1
        )
        (params1 `zip` params2)
  -- substitute the names of params in the output type
  let rtyOut1' =
        foldr
          (\((x1, _rtyIn1), (x2, _rtyIn2)) rtyOut1' -> subst rtyOut1' x1 x2)
          rtyOut1
          (params1 `zip` params2)

  -- check that the (substituted) actual output type is a subtype of the expected output type
  cstrOut <- subType l rtyOut1' rtyOut2

  let cstrOut' = foldr (uncurry (forallCstr l)) cstrOut params2

  return $ andCstr cstrIn cstrOut'
subType l rty rty' =
  throwRefining
    [ UserError
        ( pack $
            "subtyping error; actual type is '"
              <> show rty
              <> "', expected type is '"
              <> show rty'
              <> "'"
        )
        l
    ]

-- | Generated verification conditions
genQuery :: Term -> Refining Query
genQuery term =
  H.Query [] []
    <$> check emptyEnv term (IntType mempty)
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty

type Query = H.Query UserError

type Result = F.FixResult UserError

data UserError = UserError
  { userErrorMessage :: !Text,
    userErrorLabel :: !Label
  }
  deriving (Show, Typeable, Generic)

instance F.PPrint UserError where
  pprintTidy k = F.pprintTidy k . userErrorFP

instance F.Fixpoint UserError where
  toFix = PJ.text . unpack . userErrorMessage

instance F.Loc UserError where
  srcSpan = userErrorLabel

instance NFData UserError

instance Exception [UserError]

fpUserError :: F.Error1 -> UserError
fpUserError e = UserError (pack . show $ F.errMsg e) (F.errLoc e)

userErrorFP :: UserError -> F.Error
userErrorFP err =
  F.err
    (userErrorLabel err)
    (PJ.text . unpack $ userErrorMessage err)

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

resultExit :: Result -> IO ExitCode
resultExit res = do
  F.colorStrLn (F.colorResult res) (resultString res)
  case resultUserErrors res of
    [] -> return ()
    errs -> putStrLn . PJ.render =<< renderUserErrors errs
  return (F.resultExit res)

resultString :: Result -> String
resultString = \case
  F.Crash _ msg -> "Crash!: " ++ msg
  F.Unsafe {} -> "Unsafe"
  F.Safe {} -> "Safe"

resultUserErrors :: Result -> [UserError]
resultUserErrors = \case
  F.Crash errs msg -> UserError (pack $ "Crash: " <> msg) junkSpan : (fst <$> errs)
  F.Unsafe _ errs -> errs
  F.Safe {} -> []

renderUserError :: UserError -> IO PJ.Doc
renderUserError err = do
  let l = userErrorLabel err
  snippet <- readFileSpan l
  return $
    PJ.vcat
      [ F.pprint l PJ.<-> ":" PJ.<+> PJ.text (unpack $ userErrorMessage err),
        " ",
        " ",
        PJ.text snippet
      ]

renderUserErrors :: [UserError] -> IO PJ.Doc
renderUserErrors errs = do
  errs' <- mapM renderUserError errs
  return $ PJ.vcat (PJ.text "Errors found!" : PJ.text "" : errs')

readFileSpan :: F.SrcSpan -> IO String
readFileSpan span = getSpan span <$> readFile (spanFile span)

spanFile :: F.SrcSpan -> FilePath
spanFile = Misc.fst3 . F.sourcePosElts . F.sp_start

getSpan :: F.SrcSpan -> String -> String
getSpan sp
  | l1 == l2 = getSpanSingle l1 c1 c2
  | otherwise = getSpanMulti l1 l2
  where
    (_, l1, c1, l2, c2) = spanInfo sp

getSpanSingle :: Int -> Int -> Int -> String -> String
getSpanSingle l c1 c2 =
  highlight l c1 c2
    . Misc.safeHead ""
    . getRange l l
    . lines

getSpanMulti :: Int -> Int -> String -> String
getSpanMulti l1 l2 =
  highlights l1
    . getRange l1 l2
    . lines

highlight :: Int -> Int -> Int -> String -> String
highlight l c1 c2 s =
  unlines
    [ cursorLine l s,
      replicate (12 + c1) ' ' ++ replicate (1 + c2 - c1) '^'
    ]

highlights :: Int -> [String] -> String
highlights i ls = unlines $ zipWith cursorLine [i ..] ls

cursorLine :: Int -> String -> String
cursorLine l = printf "%s|  %s" (lineString l)

lineString :: Int -> String
lineString n = replicate (10 - nD) ' ' ++ nS
  where
    nS = show n
    nD = length nS

getRange :: Int -> Int -> [a] -> [a]
getRange i1 i2 =
  take (i2 - i1 + 1)
    . drop (i1 - 1)

-- | Source Span Representation
instance Semigroup F.SrcSpan where
  (<>) = mappendSpan

instance Monoid F.SrcSpan where
  mempty = junkSpan

mappendSpan :: F.SrcSpan -> F.SrcSpan -> F.SrcSpan
mappendSpan s1 s2
  | s1 == junkSpan = s2
  | s2 == junkSpan = s1
  | otherwise = F.SS (F.sp_start s1) (F.sp_stop s2)

spanInfo :: F.SrcSpan -> (FilePath, Int, Int, Int, Int)
spanInfo s = (f, F.unPos l1, F.unPos c1, F.unPos l2, F.unPos c2)
  where
    (f, l1, c1) = F.sourcePosElts (F.sp_start s)
    (_, l2, c2) = F.sourcePosElts (F.sp_stop s)

posSpan :: F.SourcePos -> F.SrcSpan
posSpan p = F.SS p p

junkSpan :: F.SrcSpan
junkSpan = F.dummySpan -- posSpan (initialPos "unknown")

-- | Main
main :: IO ()
main = do
  let l = junkSpan
  let term :: Term
      --   term = undefined -- TODO: think of example term
      -- term = ImmTerm (PrimImm (IntPrim 1) junkSpan)

      term =
        AnnTerm
          (ImmTerm (PrimImm (IntPrim 1) l))
          (IntType $ F.exprReft $ F.expr (2 :: Int))
          l
  let fp :: FilePath
      fp = "Example1.hs"
  print $ void term
  res <- case genQuery term of
    Left errs ->
      pure $
        F.Crash
          ((\err -> (err, Just . unpack . userErrorMessage $ err)) <$> errs)
          "genQuery failure"
    Right query -> checkValid fp query
  ec <- resultExit res
  exitWith ec
