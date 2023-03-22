module Language.Flex.Refining.Embedding where

import Control.Monad.Writer (MonadTrans (lift), WriterT, forM)
import Data.Foldable (foldlM, foldrM)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import Data.Text (pack)
import qualified Language.Fixpoint.Types as F
import Language.Flex.FlexM (FlexM, defaultLocated)
import Language.Flex.Refining.Prelude (tupleFTycon, tupleTermConstructorSymbol)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Syntax (Literal (..))
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), render)

-- * Embedding

embedSymId :: SymId -> RefiningM F.Expr
embedSymId SymId {..} = return $ F.eVar symIdSymbol

embedTerm :: Term (Type ()) -> RefiningM F.Expr
embedTerm = \case
  TermLiteral lit _ -> embedLiteral lit
  TermPrimitive prim _ -> embedPrimitive prim
  TermNeutral x args _ -> do
    x' <- embedSymId x
    args' <- embedTerm `traverse` args
    return
      if null args'
        then x'
        else F.eApps x' args'
  TermAssert _ tm _ -> embedTerm tm
  -- (let x = a in b) ~~> ((fun x => b) a)
  TermLet x tm bod _ -> do
    tm' <- embedTerm tm
    sort <- lift . lift . lift $ embedType (termAnn tm)
    bod' <- embedTerm bod
    return $ F.eApps (F.ELam (symIdSymbol x, sort) bod') [tm']
  TermStructure {..} -> do
    structExpr <- lift . lift . lift $ structureConstructorExpr termStructure
    termFields' <- embedTerm `traverse` (snd <$> termFields)
    return $ F.eApps structExpr termFields'

embedLiteral :: Literal -> RefiningM F.Expr
embedLiteral =
  return . \case
    Base.LiteralInteger n -> F.expr n
    Base.LiteralFloat _x -> error "TODO: embed float literal"
    Base.LiteralBit b -> if b then F.PTrue else F.PFalse
    Base.LiteralChar c -> F.expr (pack [c])
    Base.LiteralString s -> F.expr (pack s)

embedPrimitive :: Primitive (Type ()) -> RefiningM F.Expr
embedPrimitive = \case
  PrimitiveTry _ -> error "embedPrimitive Try"
  PrimitiveTuple (tm1, tm2) -> do
    -- let ty1 = termAnn tm1
    -- let ty2 = termAnn tm2
    e1 <- embedTerm tm1
    e2 <- embedTerm tm2
    -- return $ tupleConstructorExpr `F.ETApp` embedType ty1 `F.ETApp` embedType ty2 `F.EApp` e1 `F.EApp` e2
    return $ tupleConstructorExpr `F.EApp` e1 `F.EApp` e2
  PrimitiveArray _ -> error "embedPrimitive Array"
  PrimitiveIf te te' te2 -> F.EIte <$> embedTerm te <*> embedTerm te' <*> embedTerm te2
  PrimitiveAnd te te' -> F.PAnd <$> embedTerm `traverse` [te, te']
  PrimitiveOr te te' -> F.POr <$> embedTerm `traverse` [te, te']
  PrimitiveNot te -> F.PNot <$> embedTerm te
  PrimitiveEq te te' -> F.PAtom F.Eq <$> embedTerm te <*> embedTerm te'
  PrimitiveAdd te te' -> F.EBin F.Plus <$> embedTerm te <*> embedTerm te'

embedTermId :: Base.TermId -> RefiningM F.Symbol
embedTermId tmId = return $ fromString (render . pPrint $ tmId)

-- *** Primitive Constructors

tupleConstructorExpr :: F.Expr
tupleConstructorExpr = F.eVar tupleTermConstructorSymbol

-- ** Embedding as Sorts

embedType :: Type r -> FlexM F.Sort
embedType = \case
  TypeAtomic atomic _ -> case atomic of
    TypeInt -> return F.intSort
    TypeFloat -> return F.realSort
    TypeBit -> return F.boolSort
    TypeChar -> return F.charSort
    TypeString -> return F.strSort
  TypeTuple (ty1, ty2) _ -> F.fApp (F.fTyconSort tupleFTycon) <$> (embedType `traverse` [ty1, ty2])
  TypeStructure struct _ -> F.fTyconSort <$> structureFTycon struct

-- ** Datatypes

structureDataDecl :: Structure -> FlexM F.DataDecl
structureDataDecl struct@Structure {..} =
  do
    ddTyCon <- structureFTycon struct
    dcName <- structureSymbol struct
    dcFields <- forM structureFields \(fieldId, ty) -> do
      dfName <- structureFieldSymbol struct fieldId
      dfSort <- embedType ty
      return F.DField {dfName, dfSort}
    return F.DDecl {ddTyCon, ddVars = 0, ddCtors = [F.DCtor {dcName, dcFields}]}

structureSymbol :: Structure -> FlexM F.LocSymbol
structureSymbol Structure {..} = defaultLocated $ F.symbol structureId

structureFTycon :: Structure -> FlexM F.FTycon
structureFTycon Structure {..} = F.symbolFTycon <$> defaultLocated (F.symbol structureId)

-- TODO: could this cause issues since uses the same symbol as the FTycon?
structureConstructorSymbol :: Structure -> FlexM F.LocSymbol
structureConstructorSymbol = structureSymbol

structureConstructorExpr :: Structure -> FlexM F.Expr
structureConstructorExpr struct = F.eVar <$> structureConstructorSymbol struct

structureFieldSymbol :: Structure -> Base.FieldId -> FlexM F.LocSymbol
structureFieldSymbol Structure {..} fieldId = defaultLocated $ F.symbol (structureId, fieldId)
