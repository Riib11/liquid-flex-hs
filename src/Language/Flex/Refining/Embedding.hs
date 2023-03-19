module Language.Flex.Refining.Embedding where

import qualified Data.Map as Map
import Data.String (IsString (fromString))
import Data.Text (pack)
import qualified Language.Fixpoint.Types as F
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Syntax (Literal (..))
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), render)

embedId' :: Id' -> RefiningM F.Expr
embedId' Id' {..} = return $ F.eVar id'Symbol

embedTerm :: Term () -> RefiningM F.Expr
embedTerm = \case
  TermLiteral lit _ -> embedLiteral lit
  TermPrimitive prim _ -> embedPrimitive prim
  TermNeutral x args _ -> do
    x' <- embedId' x
    args' <- embedTerm `traverse` args
    return
      if null args'
        then x'
        else F.eApps x' args'
  TermAssert _ tm _ -> embedTerm tm
  -- (let x = a in b) ~~> ((fun x => b) a)
  TermLet x sort tm bod _ -> do
    tm' <- embedTerm tm
    bod' <- embedTerm bod
    return $ F.eApps (F.ELam (id'Symbol x, sort) bod') [tm']

embedType :: Type -> RefiningM F.Sort
embedType = error "embedType"

embedLiteral :: Literal -> RefiningM F.Expr
embedLiteral =
  return . \case
    Base.LiteralInteger n -> F.expr n
    Base.LiteralFloat _x -> error "TODO: embed float literal"
    Base.LiteralBit b -> if b then F.PTrue else F.PFalse
    Base.LiteralChar c -> F.expr (pack [c])
    Base.LiteralString s -> F.expr (pack s)

embedPrimitive :: Primitive () -> RefiningM F.Expr
embedPrimitive = \case
  PrimitiveTry _ -> error "embedPrimitive Try"
  PrimitiveTuple _ -> error "embedPrimitive Tuple"
  PrimitiveArray _ -> error "embedPrimitive Array"
  PrimitiveIf te te' te2 -> F.EIte <$> embedTerm te <*> embedTerm te' <*> embedTerm te2
  PrimitiveAnd te te' -> F.PAnd <$> embedTerm `traverse` [te, te']
  PrimitiveOr te te' -> F.POr <$> embedTerm `traverse` [te, te']
  PrimitiveNot te -> F.PNot <$> embedTerm te
  PrimitiveEq te te' -> F.PAtom F.Eq <$> embedTerm te <*> embedTerm te'
  PrimitiveAdd te te' -> F.EBin F.Plus <$> embedTerm te <*> embedTerm te'

embedTermId :: Base.TermId -> RefiningM F.Symbol
embedTermId tmId = return $ fromString (render . pPrint $ tmId)
