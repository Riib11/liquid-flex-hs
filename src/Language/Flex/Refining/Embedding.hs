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

embedTerm :: Term r -> RefiningM F.Expr
embedTerm = \case
  TermLiteral lit _ -> embedLiteral lit
  TermPrimitive prim _ -> embedPrimitive prim
  TermNamed x _ -> F.eVar <$> embedTermId x

embedLiteral :: Literal -> RefiningM F.Expr
embedLiteral =
  return . \case
    Base.LiteralInteger n -> F.expr n
    Base.LiteralFloat _x -> error "TODO: embed float literal"
    Base.LiteralBit b -> if b then F.PTrue else F.PFalse
    Base.LiteralChar c -> F.expr (pack [c])
    Base.LiteralString s -> F.expr (pack s)

embedPrimitive :: Primitive r -> RefiningM F.Expr
embedPrimitive = \case
  PrimitiveIf te te' te2 -> F.EIte <$> embedTerm te <*> embedTerm te' <*> embedTerm te2
  PrimitiveAnd te te' -> F.PAnd <$> embedTerm `traverse` [te, te']
  PrimitiveOr te te' -> F.POr <$> embedTerm `traverse` [te, te']
  PrimitiveNot te -> F.PNot <$> embedTerm te
  PrimitiveEq te te' -> F.PAtom F.Eq <$> embedTerm te <*> embedTerm te'

embedTermId :: Base.TermId -> RefiningM F.Symbol
embedTermId tmId = return $ fromString (render . pPrint $ tmId)
