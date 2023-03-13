{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Use lambda-case" #-}

module Language.Flex.Typing where

import Control.Category ((>>>))
import Control.Lens
import Control.Monad (forM, join)
import Control.Monad.Reader (MonadReader (ask, local), MonadTrans (lift), ReaderT, asks, foldM)
import Control.Monad.State (StateT)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Language.Flex.FlexBug as FlexBug
import Language.Flex.FlexM (FlexLog (FlexLog), FlexM)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Syntax
import Text.Printf (printf)
import Utility

-- * Typing

-- | After typing, the valid syntax for types and terms is more specific. In
-- particular, types must be normal and a few term constructors are unwrapped.
-- These properties are specified by the following predicates.

-- | Forms that are not allowed in normal types are:
-- - TypeNamed
isNormalType :: Type -> Bool
isNormalType = error "TODO"

-- | A concrete type has no type unifications variables left
isConcreteType :: Type -> Bool
isConcreteType = \case
  TypeArray ty -> isConcreteType ty
  TypeTuple tys -> isConcreteType `all` tys
  TypeOptional ty -> isConcreteType ty
  TypeUnifyVar {} -> False
  _ -> True

-- | Forms that are not allowed in typed terms are:
-- - PrimitiveCast (unwrapped)
-- - TermAscribe (unwrapped)
isTypedTerm :: Term a -> Bool
isTypedTerm = \case
  TermLiteral {} -> True
  TermPrimitive prim _ -> case prim of
    PrimitiveTry te -> isTypedTerm te
    PrimitiveCast {} -> False
    PrimitiveTuple tes -> isTypedTerm `all` tes
    PrimitiveArray tes -> isTypedTerm `all` tes
    PrimitiveIf te te1 te2 -> isTypedTerm `all` [te, te1, te2]
    PrimitiveAnd te te' -> isTypedTerm `all` [te, te']
    PrimitiveOr te te' -> isTypedTerm `all` [te, te']
    PrimitiveNot te -> isTypedTerm te
  TermBlock (stmts, tm) _ -> all isTypedStatement stmts && isTypedTerm tm
  TermStructure _ fields _ -> isTypedTerm `all` (snd <$> fields)
  TermMember te _ _ -> isTypedTerm te
  TermNeutral _ m_tes m_te's _ -> maybe True (isTypedTerm `all`) m_tes && maybe True (isTypedTerm `all`) m_te's
  TermAscribe {} -> False
  TermMatch te branches _ -> isTypedTerm te && isTypedTerm `all` (snd <$> branches)

isTypedStatement :: Statement a -> Bool
isTypedStatement = \case
  StatementLet pat te -> isTypedTerm te
  StatementAssert te -> isTypedTerm te

-- ** TypingM

type TypingM a = StateT TypingEnv (ReaderT TypingCtx FlexM) a

data TypingCtx = TypingCtx
  { _ctxTypes :: Map.Map TypeId Type,
    _ctxTerms :: Map.Map TermId (TypingM Type),
    _ctxCxparams :: Set.Set TermId
  }

data TypingEnv = TypingEnv {}

data TypingError = TypingError String (Maybe (Term ()))

makeLenses 'TypingCtx
makeLenses 'TypingEnv

-- ** TypingM utilities

introType :: TypeId -> Type -> TypingM a -> TypingM a
introType tyId ty =
  locallyM (ctxTypes . at tyId) \case
    Nothing -> return $ pure ty
    Just _ ->
      throwTypingError
        ("attempted to introduce two types with the same name: '" <> show tyId <> "'")
        Nothing

introTerm :: Bool -> TermId -> Type -> TypingM a -> TypingM a
introTerm isCxparam tmId ty = do
  comps
    [ locallyM (ctxTerms . at tmId) \case
        Nothing -> return $ pure (normType ty)
        Just _ ->
          throwTypingError
            ("attempted to introduce two top-level tersm with the same name: '" <> show tmId <> "'")
            Nothing,
      if isCxparam then locally ctxCxparams (Set.insert tmId) else id
    ]

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

throwTypingError :: String -> Maybe (Term ()) -> TypingM b
throwTypingError = error "TODO"

assertTypingProp :: String -> Maybe (Term ()) -> Bool -> TypingM ()
assertTypingProp = error "TODO"

throwTypingBug :: String -> Maybe (Term ()) -> a
throwTypingBug = error "TODO"

tellTypingLog :: String -> TypingM ()
tellTypingLog = error "TODO"

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
checkTerm ty tm = do
  tellTypingLog $ unwords ["[checkTerm]", "TODO: show tm", ":?", show ty]
  unify ty =<< join (inferTerm tm)

-- ** Synthesizing

synthTerm :: Term () -> TypingM (Term (TypingM Type))
synthTerm term = case term of
  TermLiteral lit () -> case lit of
    LiteralInteger _ ->
      TermLiteral lit <$> freshTypeUnfiyVar "literal integer" (pure $ CastedFrom $ TypeNumber TypeInt 32)
    LiteralFloat _x ->
      TermLiteral lit <$> freshTypeUnfiyVar "literal float" (pure $ CastedFrom $ TypeNumber TypeFloat 64)
    LiteralBool _b ->
      return (TermLiteral lit (return TypeBit))
    LiteralChar _c ->
      return (TermLiteral lit (return TypeChar))
    LiteralString _s ->
      return (TermLiteral lit (return (TypeArray TypeChar)))
  TermPrimitive prim () -> case prim of
    PrimitiveTry tm -> do
      -- synth type of arg, alpha
      tm' <- synthTerm tm
      -- output type is Optional alpha
      ty <- TypeOptional <$$> inferTerm tm'
      return $ TermPrimitive (PrimitiveTry tm') ty
    -- unwraps
    PrimitiveCast tm -> do
      -- synth type of arg, alpha
      tm' <- synthTerm tm
      -- output type is a fresh type unification var, with constraint that it
      -- was casted from an alpha
      ty <- freshTypeUnfiyVar "cast" . pure . CastedFrom =<<< inferTerm tm'
      -- sets the top term ann to be the result type of casting, which unwraps
      -- the cast
      return $ mapTopAnnTerm (const ty) tm'
    PrimitiveTuple tms -> do
      -- synth type of each term, alphas
      tms' <- synthTerm `traverse` tms
      -- output type is Tuple alphas
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
      return $ TermPrimitive (PrimitiveAnd tm1' tm2') (return TypeBit)
    PrimitiveOr tm1 tm2 -> do
      tm1' <- synthCheckTerm TypeBit tm1
      tm2' <- synthCheckTerm TypeBit tm2
      return $ TermPrimitive (PrimitiveOr tm1' tm2') (return TypeBit)
    PrimitiveNot tm -> do
      tm' <- synthCheckTerm TypeBit tm
      return $ TermPrimitive (PrimitiveNot tm') (return TypeBit)
  TermBlock blk () -> synthBlock blk
  TermStructure tyId fields () -> do
    ty <- lookupTypeId tyId
    struct <- case ty of
      TypeStructure struct -> return struct
      _ -> throwTypingError ("expected '" <> show tyId <> "' to be a structure type id") (pure term)
    fields' <- forM fields \(fieldId, tm) ->
      case lookup fieldId (structureFields struct) of
        Nothing -> throwTypingError ("unknown field '" <> show fieldId <> "' of the structure '" <> show tyId <> "'") (pure term)
        Just ty -> (fieldId,) <$> synthCheckTerm ty tm
    return $ TermStructure tyId fields' (normType ty)
  TermMember tm fieldId () -> do
    tm' <- synthTerm tm
    struct <-
      inferTerm tm' >>>= \case
        TypeStructure struct -> return struct
        _ -> throwTypingError ("in order to access the field '" <> show fieldId <> "', expected '" <> show tm <> "' to have a structure type") (pure term)
    ty <- case fieldId `lookup` structureFields struct of
      Nothing -> throwTypingError ("attempted to access the field '" <> show fieldId <> "', but it has structure type '" <> show (structureId struct) <> "' which does not have that field") (pure term)
      Just ty -> return (normType ty)
    return $ TermMember tm' fieldId ty
  TermNeutral tmId mb_args mb_cxargs () -> synthNeutral term tmId mb_args mb_cxargs
  -- unwraps
  TermAscribe tm ty () -> do synthCheckTerm' (normType ty) tm
  TermMatch tm branches () -> synthMatch tm branches

synthNeutral :: Term () -> TermId -> Maybe [Term ()] -> Maybe [Term ()] -> TypingM (Term (TypingM Type))
synthNeutral term tmId mb_args mb_cxargs =
  lookupTermId tmId >>= \mtype ->
    mtype >>= \type_ -> case type_ of
      -- TypeFunction
      TypeFunction func -> do
        args <- case mb_args of
          Nothing -> throwTypingError "a function application must have arguments" (pure term)
          Just args -> return args
        -- check args
        mb_args' <- pure <$> uncurry synthCheckTerm `traverse` ((snd <$> functionParameters func) `zip` args)
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
                Nothing -> throwTypingError "attempted to give explicit contextual arguments to a function that does not have contextual parameters" (pure term)
                Just cxparams -> do
                  cxargs <- synthTerm `traverse` cxargs
                  newtyIds_cxargs <- forM cxargs \tm ->
                    inferTerm tm >>>= \case
                      TypeNewtype newty -> return (newtypeId newty, tm)
                      _ -> throwTypingError "each explicit contextual argument must be a newtype" (Just term)
                  -- cxargs' is in the same order as cxparams
                  cxargs' <-
                    reverse . snd
                      <$> foldM
                        ( \(newtyIds_cxargs, cxargs') (newtyId, cxparamId) -> do
                            -- extract the cxarg that has the right newtype
                            case newtyId `lookup` newtyIds_cxargs of
                              Nothing -> throwTypingError ("missing explicit contextual argument: " <> show newtyId) (pure term)
                              Just cxarg -> return (newtyIds_cxargs, cxarg : cxargs')
                        )
                        (newtyIds_cxargs, [])
                        cxparams
                  return $ pure cxargs'
        -- output type
        let tyOut = return $ functionOutput func
        return $ TermNeutral tmId mb_args' mb_cxargs' tyOut
      -- TypeVariantConstuctor
      TypeVariantConstuctor varnt _constrId mb_tyParams -> do
        -- check arguments
        mb_args' <- case mb_tyParams of
          -- expects no arguments
          Nothing -> do
            case mb_args of
              Just _ -> throwTypingError "the variant constructor expects no arguments, but some were given" (pure term)
              Nothing -> return Nothing
          -- expects some arguments
          Just tyParams -> do
            case mb_args of
              Nothing -> throwTypingError "the variant constructor expects some arguments, but none were given" (pure term)
              Just args ->
                pure <$> uncurry synthCheckTerm `traverse` (tyParams `zip` args)
        -- check contextual args (can't have any)
        assertTypingProp "a variant constructor can't have contextual arguments" (pure term) (isNothing mb_cxargs)
        -- output type
        let tyOut = return $ TypeVariant varnt
        return $ TermNeutral tmId mb_args' Nothing tyOut
      -- TypeEnumConstructor
      TypeEnumConstructor enume constrId -> do
        -- check arguments (can't have any)
        assertTypingProp "cannot apply an enum constructor" (pure term) (isNothing mb_args)
        -- check contextual args (can't have any)
        assertTypingProp "cannot give contextual argsuments to an enum constructor" (pure term) (isNothing mb_cxargs)
        -- output type
        let tyOut = return $ TypeEnumerated enume
        return $ TermNeutral tmId Nothing Nothing tyOut
      -- TypeNewtypeConstructor
      TypeNewtypeConstructor newty -> do
        -- check argument (must have exactly 1)
        mb_args' <- case mb_args of
          Just [arg] ->
            pure . pure <$> synthCheckTerm (newtypeType newty) arg
          Just _ -> throwTypingError "a newtype constructor requires exactly one argument" (Just term)
          Nothing -> throwTypingError "a newtype constructor must be given an argument" (Just term)
        assertTypingProp "a newtype constructor cannot be given contextual arguments" (pure term) (isNothing mb_cxargs)
        -- output type
        let tyOut = return $ TypeNewtype newty
        return $ TermNeutral tmId mb_args' Nothing tyOut
      -- non-functional type
      _ -> do
        assertTypingProp ("cannot give arguments to a term of type '" <> show type_ <> "'") (pure term) (isNothing mb_args)
        assertTypingProp ("cannot give contextual argument to a term of type '" <> show type_ <> "'") (pure term) (isNothing mb_cxargs)
        return $ TermNeutral tmId Nothing Nothing mtype

synthBlock :: Block () -> TypingM (Term (TypingM Type))
synthBlock (stmts, tm) = error "synthBlock"

synthMatch :: Term () -> Branches () -> TypingM (Term (TypingM Type))
synthMatch tm branches = error "synthMatch"

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
