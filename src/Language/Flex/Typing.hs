{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.Typing where

import Control.Lens
import Control.Monad (forM)
import Control.Monad.Reader (MonadTrans (lift), ReaderT, foldM)
import Control.Monad.State (StateT)
import Language.Flex.FlexM (FlexLog (FlexLog), FlexM, throwFlexBug)
import Language.Flex.Syntax
import Utility

-- * Typing

-- ** TypingM

type TypingM a = StateT TypingEnv (ReaderT TypingCtx FlexM) a

data TypingCtx = TypingCtx {}

data TypingEnv = TypingEnv {}

makeLenses 'TypingCtx
makeLenses 'TypingEnv

-- ** Processing

procModule :: Module () -> TypingM (Module Type)
procModule mdl = error "procModule"

procDeclaration :: Declaration () -> TypingM (Declaration Type)
procDeclaration decl = error "procDeclaration"

-- ** Checking

checkTerm' :: TypingM Type -> Term () -> TypingM (Term (TypingM Type))
checkTerm' m_ty tm = do
  ty <- m_ty
  checkTerm ty tm

checkTerm :: Type -> Term () -> TypingM (Term (TypingM Type))
checkTerm ty tm = do
  tm' <- synthTerm tm
  error "checkTerm"

-- ** Synthesizing

synthTerm :: Term () -> TypingM (Term (TypingM Type))
synthTerm = \case
  TermLiteral lit _ -> case lit of
    LiteralInteger _ -> TermLiteral lit <$> freshTypeUnfiyVar "literal integer" (Just $ CastedFrom $ TypeNumber TypeInt 32)
    LiteralFloat x -> TermLiteral lit <$> freshTypeUnfiyVar "literal float" (Just $ CastedFrom $ TypeNumber TypeFloat 64)
    LiteralBool b -> return (normType <$> TermLiteral lit TypeBit)
    LiteralChar c -> return (normType <$> TermLiteral lit TypeChar)
    LiteralString s -> return (normType <$> TermLiteral lit (TypeArray TypeChar))
  TermPrimitive prim _ -> case prim of
    PrimitiveTry tm -> do
      tm' <- synthTerm tm
      ty <- TypeOptional <$$> inferTerm tm'
      return $ TermPrimitive (PrimitiveTry tm') ty
    PrimitiveCast tm -> do
      tm' <- synthTerm tm
      ty <- freshTypeUnfiyVar "cast" . Just . CastedFrom =<<< inferTerm tm'
      return $ TermPrimitive (PrimitiveCast tm') ty
    PrimitiveTuple tms -> do
      tms' <- synthTerm `traverse` tms
      tys <- inferTerm `traverse` tms'
      return $ TermPrimitive (PrimitiveTuple tms') (TypeTuple <$> sequence tys)
    PrimitiveArray [] -> do
      -- empty array introduces a new type unification variable
      ty <- freshTypeUnfiyVar "empty array" Nothing
      return $ TermPrimitive (PrimitiveTuple []) (TypeArray <$> ty)
    PrimitiveArray (tm : tms) -> do
      -- synthesize first term
      tm' <- synthTerm tm
      ty <- inferTerm tm'
      -- check the rest of terms against that type
      tms' <- forM tms \tm -> do
        checkTerm' ty tm
      return $ TermPrimitive (PrimitiveArray (tm' : tms')) (TypeArray <$> ty)
    PrimitiveIf tm tm' tm2 -> error "TODO"
    PrimitiveAnd tm tm' -> error "TODO"
    PrimitiveOr tm tm' -> error "TODO"
    PrimitiveNot tm -> error "TODO"
  TermNamed ti _ -> error "TODO"
  TermBlock _ x1 -> error "TODO"
  TermStructure ti map _ -> error "TODO"
  TermMember tm fi _ -> error "TODO"
  TermApplication ti tms m_tms _ -> error "TODO"
  TermAscribe tm ty _ -> error "TODO"
  TermMatch tm _ x1 -> error "TODO"

-- ** Normalization

normType :: Type -> TypingM Type
normType ty = error "normType"

normTerm :: Term Type -> TypingM (Term Type)
normTerm = traverse normType

-- ** getType

inferTerm :: Term (TypingM Type) -> TypingM (TypingM Type)
inferTerm =
  return . \case
    TermLiteral _ ty -> normType =<< ty
    TermNamed _ ty -> normType =<< ty
    TermBlock _ ty -> normType =<< ty
    TermStructure _ _ ty -> normType =<< ty
    TermMember _ _ ty -> normType =<< ty
    TermAscribe _ _ ty -> normType =<< ty
    TermMatch _ _ ty -> normType =<< ty
    TermApplication _ _ _ ty -> normType =<< ty
    TermPrimitive _ ty -> normType =<< ty

-- ** TypeUnifVar

freshTypeUnfiyVar :: String -> Maybe UnifyConstraint -> TypingM (TypingM Type)
freshTypeUnfiyVar str mb_constr = do
  error "TODO: fresh unification variable id"
  return do
    error "TODO: normalize"

-- ** Unification

unify' :: TypingM Type -> TypingM Type -> TypingM ()
unify' m_ty1 m_ty2 = do
  ty1 <- m_ty1
  ty2 <- m_ty2
  unify ty1 ty2

unify :: Type -> Type -> TypingM ()
unify = error "checkUnify"
