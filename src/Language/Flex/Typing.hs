{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Use lambda-case" #-}

module Language.Flex.Typing where

import Control.Category ((>>>))
import Control.Lens
import Control.Monad (forM, join)
import Control.Monad.Reader (MonadTrans (lift), ReaderT, asks, foldM)
import Control.Monad.State (StateT)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Language.Flex.FlexM (FlexLog (FlexLog), FlexM, throwFlexBug, throwFlexError)
import Language.Flex.Syntax
import Utility

-- * Typing

throwTypingError :: String -> Maybe (Term a) -> TypingM b
throwTypingError = error "TODO"

assertTypingProp :: String -> Bool -> TypingM ()
assertTypingProp = error "TODO"

-- ** TypingM

type TypingM a = StateT TypingEnv (ReaderT TypingCtx FlexM) a

data TypingCtx = TypingCtx
  { _ctxTypes :: Map.Map TypeId Type,
    _ctxTerms :: Map.Map TermId (TypingM Type),
    _ctxCxparams :: Set.Set TermId
  }

data TypingEnv = TypingEnv {}

makeLenses 'TypingCtx
makeLenses 'TypingEnv

-- ** Interacting with TypingCtx

introType :: TypeId -> Type -> TypingM a -> TypingM a
introType tyId ty =
  locally (ctxTypes . at tyId) \case
    Nothing -> Just ty
    Just _ ->
      throwFlexError $
        FlexLog "typing" $
          "attempted to introduce two types with the same name: '" <> show tyId <> "'"

introTerm :: Bool -> TermId -> Type -> TypingM a -> TypingM a
introTerm isCxparam tmId ty =
  ( locally (ctxTerms . at tmId) \case
      Nothing -> Just (normType ty)
      Just _ ->
        throwFlexError $
          FlexLog "typing" $
            "attempted to introduce two top-level tersm with the same name: '" <> show tmId <> "'"
  )
    >>> if isCxparam then locally ctxCxparams (Set.insert tmId) else id

lookupTypeId :: TypeId -> TypingM Type
lookupTypeId tyId =
  asks (^. ctxTypes . at tyId)
    >>= \case
      Just ty -> return ty
      Nothing -> throwTypingError ("unknown type id '" <> show tyId <> "'") Nothing

lookupTermId :: TermId -> TypingM (TypingM Type)
lookupTermId tmId =
  asks (^. ctxTerms . at tmId) >>= \case
    Just mty -> return mty
    Nothing -> throwTypingError ("unknown term id '" <> show tmId <> "'") Nothing

-- ** Top TypingCtx and TypingEnv

topTypingCtx :: Module () -> TypingCtx
topTypingCtx = undefined

topTypingEnv :: Module () -> TypingEnv
topTypingEnv = undefined

-- ** Processing

procModule :: Module () -> TypingM (Module Type)
procModule mdl = error "procModule"

procDeclaration :: Declaration () -> TypingM (Declaration Type)
procDeclaration decl = error "procDeclaration"

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
synthTerm term = case term of
  TermLiteral lit () -> case lit of
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
  TermPrimitive prim () -> case prim of
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
  TermBlock blk () -> synthBlock blk
  TermStructure tyId fields () -> do
    ty <- lookupTypeId tyId
    struct <- case ty of
      TypeStructure struct -> return struct
      _ -> throwTypingError ("expected '" <> show tyId <> "' to be a structure type id") (Just term)
    fields' <- forM fields \(fieldId, tm) ->
      case lookup fieldId (structureFields struct) of
        Nothing -> throwTypingError ("unknown field '" <> show fieldId <> "' of the structure '" <> show tyId <> "'") (Just term)
        Just ty -> (fieldId,) <$> synthCheckTerm ty tm
    return $ TermStructure tyId fields' (normType ty)
  TermMember tm fieldId () -> do
    tm' <- synthTerm tm
    struct <-
      inferTerm tm' >>>= \case
        TypeStructure struct -> return struct
        _ -> throwTypingError ("in order to access the field '" <> show fieldId <> "', expected '" <> show tm <> "' to have a structure type") (Just term)
    ty <- case fieldId `lookup` structureFields struct of
      Nothing -> throwTypingError ("attempted to access the field '" <> show fieldId <> "' from term '" <> "TODO: show tm'" <> "', but it has structure type '" <> show (structureId struct) <> "' which does not have that field") (Just term)
      Just ty -> return (normType ty)
    return $ TermMember tm' fieldId ty
  TermNeutral tmId args mb_cxargs () -> do
    lookupTermId tmId >>>= \ty -> case ty of
      TypeFunction func -> do
        -- check arg
        args' <- uncurry synthCheckTerm `traverse` ((snd <$> functionParameters func) `zip` args)
        -- check cxargs
        mb_cxargs' <-
          case mb_cxargs of
            -- implicitly give, or no cxargs
            Nothing -> do
              case functionContextualParameters func of
                -- no cxparams
                Nothing -> return Nothing
                -- implicit cxparams
                Just cxparams -> error "TODO: infer cxargs based on whats among local vars"
            Just cxargs -> do
              case functionContextualParameters func of
                -- actually, no cxparams!
                Nothing -> throwTypingError "attempted to give explicit contextual arguments to a function that does not have contextual parameters" (Just term)
                Just cxparams -> do
                  cxargs <- synthTerm `traverse` cxargs
                  newtyIds_cxargs <- forM cxargs \tm ->
                    inferTerm tm >>>= \case
                      TypeNewtype newty -> return (newtypeId newty, tm)
                      _ -> undefined
                  -- cxargs' is in the same order as cxparams
                  cxargs' <-
                    reverse . snd
                      <$> foldM
                        ( \(newtyIds_cxargs, cxargs') (newtyId, cxparamId) -> do
                            -- extract the cxarg that has the right newtype
                            case newtyId `lookup` newtyIds_cxargs of
                              Nothing -> throwTypingError ("missing explicit contextual argument: " <> show newtyId) (Just term)
                              Just cxarg -> return (newtyIds_cxargs, cxarg : cxargs')
                        )
                        (newtyIds_cxargs, [])
                        cxparams
                  return $ Just cxargs'
        -- result type
        let tyOut = normType (functionOutput func)
        return $ TermNeutral tmId args' mb_cxargs' tyOut
      TypeVariantConstuctor varnt ti -> undefined
      TypeEnumConstructor enume ti -> undefined
      TypeNewtypeConstructor newty ti -> undefined
      _ -> do
        assertTypingProp
          ("cannot apply a term of type '" <> show ty <> "'")
          (null args && isNothing mb_cxargs)
        error "TODO"
  TermAscribe tm ty () -> error "TODO"
  TermMatch tm _ () -> error "TODO"

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
