{-# LANGUAGE TemplateHaskell #-}

module Language.Flex.Typing where

import Control.Category ((>>>))
import Control.Lens
import Control.Monad (forM, join)
import Control.Monad.Reader (MonadTrans (lift), ReaderT, foldM)
import Control.Monad.State (StateT)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Flex.FlexM (FlexLog (FlexLog), FlexM, throwFlexBug)
import Language.Flex.Syntax
import Utility

-- * Typing

-- ** TypingM

type TypingM a = StateT TypingEnv (ReaderT TypingCtx FlexM) a

data TypingCtx = TypingCtx
  { _types :: Map.Map TypeId Type,
    _terms :: Map.Map TermId Type,
    _localTerms :: Set.Set TermId
  }

data TypingEnv = TypingEnv {}

makeLenses 'TypingCtx
makeLenses 'TypingEnv

-- ** Interacting with TypingCtx

introType :: TypeId -> Type -> TypingM a -> TypingM a
introType tyId ty =
  locally (types . at tyId) \case
    Nothing -> Just ty
    Just _ ->
      throwFlexBug $
        FlexLog "typing" $
          "attempted to introduce two types with the same name: '" <> show tyId <> "'"

introTerm :: Bool -> TermId -> Type -> TypingM a -> TypingM a
introTerm isLocal tmId ty =
  ( locally (terms . at tmId) \case
      Nothing -> Just ty
      Just _ ->
        throwFlexBug $
          FlexLog "typing" $
            "attempted to introduce two top-level tersm with the same name: '" <> show tmId <> "'"
  )
    >>> if isLocal then locally localTerms (Set.insert tmId) else id

-- ** Processing

-- procModule :: Module () -> TypingM (Module Type)
-- procModule mdl = error "procModule"

-- procDeclaration :: Declaration () -> TypingM (Declaration Type)
-- procDeclaration decl = error "procDeclaration"

procModule :: Module () -> FlexM (Module Type)
procModule mdl = do
  -- first, create the
  error "procModule"

-- ** Synthesizing then Checking

synthCheckTerm' :: TypingM Type -> Term () -> TypingM (Term (TypingM Type))
synthCheckTerm' m_ty tm = do
  ty <- m_ty
  synthCheckTerm ty tm

synthCheckTerm :: Type -> Term () -> TypingM (Term (TypingM Type))
synthCheckTerm ty tm = do
  tm' <- synthTerm tm
  checkTerm ty tm'
  return tm'

-- ** Checking

checkTerm :: Type -> Term (TypingM Type) -> TypingM ()
checkTerm ty tm = unify ty =<< join (inferTerm tm)

-- ** Synthesizing

synthTerm :: Term () -> TypingM (Term (TypingM Type))
synthTerm = \case
  TermLiteral lit _ -> case lit of
    LiteralInteger _ ->
      TermLiteral lit <$> freshTypeUnfiyVar "literal integer" (Just $ CastedFrom $ TypeNumber TypeInt 32)
    LiteralFloat _x ->
      TermLiteral lit <$> freshTypeUnfiyVar "literal float" (Just $ CastedFrom $ TypeNumber TypeFloat 64)
    LiteralBool _b ->
      return (TermLiteral lit (normType TypeBit))
    LiteralChar _c ->
      return (TermLiteral lit (normType TypeChar))
    LiteralString _s ->
      return (TermLiteral lit (normType (TypeArray TypeChar)))
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
      -- synthesize type of first term
      tm' <- synthTerm tm
      ty <- inferTerm tm'
      -- check the rest of terms against that type
      tms' <- forM tms (synthCheckTerm' ty)
      return $ TermPrimitive (PrimitiveArray (tm' : tms')) (TypeArray <$> ty)
    PrimitiveIf tm tm1 tm2 -> do
      tm' <- synthCheckTerm TypeBit tm
      -- synthesize type of first branch
      tm1' <- synthTerm tm1
      ty <- inferTerm tm1'
      -- check other branch against that type
      tm2' <- synthCheckTerm' ty tm2
      return $ TermPrimitive (PrimitiveIf tm' tm1' tm2') ty
    PrimitiveAnd tm1 tm2 -> do
      tm1' <- synthCheckTerm TypeBit tm1
      tm2' <- synthCheckTerm TypeBit tm2
      return $ TermPrimitive (PrimitiveAnd tm1' tm2') (normType TypeBit)
    PrimitiveOr tm1 tm2 -> do
      tm1' <- synthCheckTerm TypeBit tm1
      tm2' <- synthCheckTerm TypeBit tm2
      return $ TermPrimitive (PrimitiveOr tm1' tm2') (normType TypeBit)
    PrimitiveNot tm -> do
      tm' <- synthCheckTerm TypeBit tm
      return $ TermPrimitive (PrimitiveNot tm') (normType TypeBit)
  TermBlock blk _ -> synthBlock blk
  TermStructure tyId fields _ -> do
    error "TODO"
  TermMember tm fi _ -> error "TODO"
  TermNeutral ti tms m_tms _ -> error "TODO"
  TermAscribe tm ty _ -> error "TODO"
  TermMatch tm _ x1 -> error "TODO"

synthBlock :: Block () -> TypingM (Term (TypingM Type))
synthBlock (stmts, tm) = error "synthBlock"

-- ** Normalization

normType :: Type -> TypingM Type
normType ty = error "normType"

normTerm :: Term Type -> TypingM (Term Type)
normTerm = traverse normType

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

-- ** Inference

inferTerm :: Term (TypingM Type) -> TypingM (TypingM Type)
inferTerm =
  return . \case
    TermLiteral _ ty -> normType =<< ty
    TermBlock _ ty -> normType =<< ty
    TermStructure _ _ ty -> normType =<< ty
    TermMember _ _ ty -> normType =<< ty
    TermAscribe _ _ ty -> normType =<< ty
    TermMatch _ _ ty -> normType =<< ty
    TermNeutral _ _ _ ty -> normType =<< ty
    TermPrimitive _ ty -> normType =<< ty