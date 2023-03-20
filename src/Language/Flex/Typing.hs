{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Language.Flex.Typing where

import Control.Category ((>>>))
import Control.Lens hiding (enum, (<&>)) -- (At (at), makeLenses, modifying, to, (^.))
-- (At (at), makeLenses, modifying, to, (^.))
import Control.Monad (forM, join, unless, void, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadReader (ask, local), MonadTrans (lift), ReaderT (runReaderT), asks, foldM)
import Control.Monad.State (StateT (runStateT), gets)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Language.Flex.FlexBug as FlexBug
import Language.Flex.FlexM (FlexLog (FlexLog), FlexM)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Syntax
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Text.Printf (printf)
import Utility
import Prelude hiding (Enum)

-- * Typing

-- ** Utility Types

-- | During typing, store each type as a `TypeM = TypingM Type` so that whenever
-- a `Type`'s value is used, the `TypingM` must be run first (in a `TypingM`
-- computation), which normalizes the internal `Type` via the contextual
-- unification substitution.
type TypeM = TypingM Type

-- ** TypingM

type TypingM = StateT TypingEnv (ReaderT TypingCtx (ExceptT TypingError FlexM))

data TypingCtx = TypingCtx
  { _ctxTypes :: Map.Map TypeId TypeM,
    _ctxApplicants :: Map.Map (Applicant ()) (Applicant TypeM),
    _ctxCxparamNewtypes :: Map.Map TermId TypeId,
    _ctxCxparams :: Map.Map TypeId TermId
  }

-- data ApplicantTypeM
--   = ApplicantTypeFunction (TypingM FunctionType)
--   | ApplicantTypeEnumConstructor Enum TermId
--   | ApplicantTypeVariantConstructor (TypingM Variant) TermId [TypeM]
--   | ApplicantTypeNewtypeConstructor (TypingM Newtype)
--   | ApplicantType TypeM

data TypingEnv = TypingEnv
  { -- | unifying substitution
    _envUnification :: Map.Map UnifyVar Type,
    _envFreshUnificationVarIndex :: Int
  }

data TypingError = TypingError Doc (Maybe (Syntax ()))

instance Pretty TypingError where
  pPrint (TypingError msg mb_src) =
    text "typing error:"
      <+> msg
      $$ maybe mempty (nest 2 . pPrint) mb_src

makeLenses 'TypingCtx
makeLenses 'TypingEnv

-- ** TypingM utilities

introTerm :: TermId -> TypeM -> TypingM a -> TypingM a
introTerm tmId tyM =
  locallyM (ctxApplicants . at (Applicant Nothing tmId (ApplicantType ()))) \case
    Nothing -> return . pure $ Applicant Nothing tmId (ApplicantType tyM)
    Just _ -> throwTypingError ("attempted to introduce two terms with the same name:" <+> ticks (pPrint tmId)) Nothing

introCxparam :: Maybe (Syntax ()) -> TypeId -> TermId -> TypingM a -> TypingM a
introCxparam mb_syn tyId tmId =
  comps
    [ introTerm tmId (normType (TypeNamed tyId)),
      locallyM (ctxCxparamNewtypes . at tmId) \case
        Nothing -> return $ pure tyId
        Just _ -> throwTypingError ("attempted to introduce two contextual parameters with the same name:" <+> ticks (pPrint tmId)) mb_syn,
      locallyM (ctxCxparams . at tyId) \case
        Nothing -> return $ pure tmId
        Just _ -> throwTypingError ("attempted to introduce two contextual parameters with the same type:" <+> ticks (pPrint tyId)) mb_syn
    ]

lookupApplicant :: Applicant () -> TypingM (Applicant TypeM)
lookupApplicant app =
  asks (^. ctxApplicants . at app) >>= \case
    Nothing -> throwTypingError ("unknown applicant:" <+> ticks (pPrint app)) Nothing
    Just appTyM -> return appTyM

lookupType :: TypeId -> TypingM TypeM
lookupType tyId =
  asks (^. ctxTypes . at tyId)
    >>= \case
      Nothing -> throwTypingError ("unknown type id" <+> ticks (pPrint tyId)) Nothing
      Just tyM -> return tyM

throwTypingError :: MonadError TypingError m => Doc -> Maybe (Syntax ()) -> m b
throwTypingError err mb_syn = throwError $ TypingError err mb_syn

tellTypingLog :: Doc -> TypingM ()
tellTypingLog body =
  FlexM.tell $
    FlexLog
      { logLabel = "typing",
        logBody = body
      }

-- ** Top TypingCtx and TypingEnv

topTypingCtx :: forall m. MonadError TypingError m => Module () -> m TypingCtx
topTypingCtx Module {..} = do
  let structs = undefined :: Map.Map TypeId Structure -- TODO:
  foldlM'
    moduleDeclarations
    TypingCtx
      { _ctxTypes = mempty,
        _ctxApplicants = mempty,
        _ctxCxparamNewtypes = mempty,
        _ctxCxparams = mempty
      }
    \ctx decl -> do
      let addType :: TypeId -> TypeM -> TypingCtx -> m TypingCtx
          addType tyId tyM ctx' = case ctx' ^. ctxTypes . at tyId of
            Just _ -> throwTypingError ("conflicting definitions of type:" <+> ticks (pPrint tyId)) (pure . toSyntax $ decl)
            Nothing -> return $ ctx' & ctxTypes . at tyId ?~ tyM

      -- let addTerm :: TermId -> TypeM -> TypingCtx -> m TypingCtx
      --     addTerm tmId ty ctx' = case ctx' ^. ctxApplicant . at tmId of
      --       Nothing -> return $ ctx' & ctxTerms . at tmId ?~ ty
      --       Just _ -> throwTypingError ("conflicting definitions of term:" <+> ticks (pPrint tmId)) (pure . toSyntax $ decl)

      let addApplicant :: Applicant TypeM -> TypingCtx -> m TypingCtx
          addApplicant app ctx' = case ctx' ^. ctxApplicants . at (void app) of
            Nothing -> return $ ctx' & ctxApplicants . at (void app) ?~ app
            Just _ -> throwTypingError ("conflicting definitions of:" <+> ticks (pPrint app)) (pure . toSyntax $ decl)

      case decl of
        DeclarationStructure struct@Structure {..} -> do
          struct' <- inlineStructureExtension structs [] struct
          (ctx &) $
            addType structureId (normType $ TypeStructure struct')
        DeclarationNewtype newty@Newtype {..} -> do
          (ctx &) . compsM $
            [ addType newtypeId (normType $ TypeNewtype newty),
              addApplicant
                ( Applicant Nothing newtypeConstructorId $
                    ApplicantTypeNewtypeConstructor (normType <$> newty)
                )
            ]
        DeclarationVariant varnt@Variant {..} ->
          (ctx &) . compsM . concat $
            [ [addType variantId (normType $ TypeVariant varnt)],
              variantConstructors <&> \(constrId, tyParams) ->
                addApplicant
                  Applicant
                    { applicantMaybeTypeId = Just variantId,
                      applicantTermId = constrId,
                      applicantAnn =
                        ApplicantTypeVariantConstructor
                          (normType <$> varnt)
                          constrId
                          (normType <$> tyParams)
                    }
            ]
        DeclarationEnum enum@Enum {..} ->
          (ctx &) . compsM . concat $
            [ [addType enumId (normType $ TypeEnum enum)],
              enumConstructors <&> \(constrId, _) ->
                addApplicant
                  Applicant
                    { applicantMaybeTypeId = Just enumId,
                      applicantTermId = constrId,
                      applicantAnn =
                        ApplicantTypeEnumConstructor
                          (normType <$> enum)
                          constrId
                    }
            ]
        DeclarationAlias Alias {..} ->
          (ctx &) $
            addType aliasId (normType aliasType)
        DeclarationFunction Function {..} -> do
          (ctx &) $
            addApplicant
              Applicant
                { applicantMaybeTypeId = Nothing,
                  applicantTermId = functionId,
                  applicantAnn =
                    ApplicantTypeFunction
                      FunctionType
                        { functionTypeId = functionId,
                          functionTypeIsTransform = functionIsTransform,
                          functionTypeParameters = second normType <$> functionParameters,
                          functionTypeContextualParameters = functionContextualParameters,
                          functionTypeOutput = normType functionOutput
                        }
                }
        DeclarationConstant Constant {..} ->
          (ctx &) $
            addApplicant
              Applicant
                { applicantMaybeTypeId = Nothing,
                  applicantTermId = constantId,
                  applicantAnn = ApplicantType (normType constantType)
                }
        DeclarationRefinedType RefinedType {} ->
          return ctx

topTypingEnv :: MonadError TypingError m => Module () -> m TypingEnv
topTypingEnv _mdl =
  return
    TypingEnv
      { _envUnification = mempty,
        _envFreshUnificationVarIndex = 0
      }

-- | Fill structure fields with inherited fields
inlineStructureExtension :: MonadError TypingError m => Map.Map TypeId Structure -> [TypeId] -> Structure -> m Structure
inlineStructureExtension structs extIdStack struct = do
  when (structureId struct `elem` extIdStack) $
    throwTypingError
      ("structure extension cycle:" <+> hsep (punctuate (text "extends") (pPrint <$> extIdStack)))
      (pure $ toSyntax $ toDeclaration @_ @() $ struct)
  case structureMaybeExtensionId struct of
    Nothing -> return struct
    Just extId ->
      case Map.lookup extId structs of
        Nothing -> throwTypingError ("unknown structure id" <+> ticks (pPrint extId)) (pure $ toSyntax $ toDeclaration @_ @() $ struct)
        Just structExt -> do
          structExt' <- inlineStructureExtension structs (structureId struct : extIdStack) structExt
          -- merge fields
          structureFields <-
            foldM
              ( \fields (fieldId, ty) ->
                  case fieldId `lookup` fields of
                    Nothing -> return $ fields @ (fieldId, ty)
                    Just _ -> throwTypingError ("attempted to extend structure by another structure with conflicting field" <+> ticks (pPrint fieldId)) (pure $ toSyntax $ toDeclaration @_ @() $ struct)
              )
              (structureFields struct)
              (structureFields structExt')
          return struct {structureFields}

typeModule :: Module () -> FlexM (Either TypingError (Module Type, TypingEnv))
typeModule mdl = do
  case topTypingCtx mdl of
    Left err -> return . Left $ err
    Right r -> case topTypingEnv mdl of
      Left err -> return . Left $ err
      Right s -> do
        runExceptT $ flip runReaderT r $ flip runStateT s $ do
          mdl' <- procModule mdl
          mdl'' <- defaultType `traverse` mdl'
          assertNormalModule mdl''
          return mdl''

-- ** Processing

procModule :: Module () -> TypingM (Module Type)
procModule (Module {..}) = do
  decls <- procDeclaration `traverse` moduleDeclarations
  return $ Module {moduleId, moduleDeclarations = decls}

procDeclaration :: Declaration () -> TypingM (Declaration Type)
procDeclaration decl = case decl of
  DeclarationStructure (Structure {..}) ->
    return . toDeclaration $
      Structure
        { structureId,
          structureIsMessage,
          structureMaybeExtensionId,
          structureFields
        }
  DeclarationNewtype (Newtype {..}) ->
    return . toDeclaration $
      Newtype
        { newtypeId,
          newtypeConstructorId,
          newtypeFieldId,
          newtypeType
        }
  DeclarationVariant (Variant {..}) ->
    return . toDeclaration $
      Variant
        { variantId,
          variantConstructors
        }
  DeclarationEnum (Enum {..}) ->
    return . toDeclaration $
      Enum
        { enumId,
          enumType,
          enumConstructors
        }
  DeclarationAlias (Alias {..}) ->
    return . toDeclaration $
      Alias
        { aliasId,
          aliasType
        }
  DeclarationFunction (Function {..}) -> do
    body <-
      sequence
        =<< (
              -- intro params
              foldr'
                ( \(tmIdArg, tyArg) m -> do
                    -- check that transform's param types are messages
                    when functionIsTransform do
                      tyArg
                        >>= \case
                          TypeStructure Structure {..} | structureIsMessage -> return ()
                          _ -> throwTypingError "a transform can have only message type parameters" (pure . toSyntax $ decl)

                    introTerm tmIdArg tyArg m
                )
                (second normType <$> functionParameters)
                $
                -- intro contextual params
                maybe id (foldr' (uncurry (introCxparam (pure $ toSyntax decl)))) functionContextualParameters
                $ synthCheckTerm functionOutput functionBody
            )
    return . toDeclaration $
      Function
        { functionId,
          functionIsTransform,
          functionParameters,
          functionContextualParameters,
          functionOutput,
          functionBody = body
        }
  DeclarationConstant (Constant {..}) -> do
    ty <- normType constantType
    body <- sequence =<< synthCheckTerm ty constantBody
    return . toDeclaration $
      Constant
        { constantId,
          constantType = ty,
          constantBody = body
        }
  DeclarationRefinedType (RefinedType {..}) -> do
    let m_rfn = sequence =<< synthRefinement refinedTypeRefinement
    rfn <-
      lookupType refinedTypeId >>>= \case
        TypeStructure struct ->
          foldr'
            ( \(fieldId, ty) ->
                introTerm (fromFieldIdToTermId fieldId) (normType ty)
            )
            (structureFields struct)
            m_rfn
        TypeNewtype newty ->
          introTerm
            (fromFieldIdToTermId $ newtypeFieldId newty)
            (normType $ newtypeType newty)
            m_rfn
        ty -> throwTypingError ("cannot declare refinement for" <+> pPrint ty <+> "; can only refine structures and newtypes") (pure . toSyntax $ decl)
    return . DeclarationRefinedType $
      RefinedType
        { refinedTypeId,
          refinedTypeRefinement = rfn
        }

-- ** Synthesizing then Checking

synthCheckTerm' :: TypeM -> Term () -> TypingM (Term TypeM)
synthCheckTerm' m_ty tm = do
  ty <- m_ty
  synthCheckTerm ty tm

synthCheckTerm :: Type -> Term () -> TypingM (Term TypeM)
synthCheckTerm ty tm = do
  tm' <- synthTerm tm
  checkTerm ty tm'
  return tm'

-- ** Checking

checkTerm :: Type -> Term TypeM -> TypingM ()
checkTerm ty tm = do
  tellTypingLog $ hsep ["[checkTerm]", pPrint tm, ":?", pPrint ty]
  unify ty =<< join (inferTerm tm)

checkPattern' :: TypingM Type -> Pattern () -> TypingM (Pattern TypeM)
checkPattern' tyM pat = do
  ty <- tyM
  checkPattern ty pat

checkPattern :: Type -> Pattern () -> TypingM (Pattern TypeM)
checkPattern ty = \case
  PatternNamed tmId () -> return $ PatternNamed tmId (normType ty)
  PatternDiscard () -> return $ PatternDiscard (normType ty)

-- ** Synthesizing

synthTerm :: Term () -> TypingM (Term TypeM)
synthTerm term = case term of
  TermLiteral lit () -> TermLiteral lit <$> typeLiteral lit
  TermPrimitive prim () -> synthPrimitive term prim
  TermLet {termPattern, termTerm, termBody} -> do
    tm <- synthTerm termTerm
    sig <- inferTerm tm
    pat <- checkPattern' sig termPattern
    bod <- case pat of
      PatternNamed tmId _ -> introTerm tmId sig $ synthTerm termBody
      PatternDiscard _ -> synthTerm termBody
    ty <- inferTerm bod
    return $ TermLet pat tm bod ty
  TermAssert {termTerm, termBody} -> do
    tm <- synthCheckTerm' (normType TypeBit) termTerm
    bod <- synthTerm termBody
    ty <- inferTerm bod
    return $ TermAssert tm bod ty
  TermStructure tyId fields () -> do
    struct <-
      lookupType tyId >>>= \case
        TypeStructure struct -> return struct
        _ -> throwTypingError ("expected" <+> ticks (pPrint tyId) <+> " to be a structure type id") (pure . toSyntax $ term)
    fields' <- forM fields \(fieldId, tm) ->
      case lookup fieldId (structureFields struct) of
        Nothing -> throwTypingError ("unknown field" <+> pPrint fieldId <+> " of the structure" <+> ticks (pPrint tyId)) (pure . toSyntax $ term)
        Just tyField -> (fieldId,) <$> synthCheckTerm tyField tm
    return $ TermStructure tyId fields' (return $ TypeStructure struct)
  TermMember tm fieldId () -> do
    tm' <- synthTerm tm
    struct <-
      inferTerm tm' >>>= \case
        TypeStructure struct -> return struct
        _ -> throwTypingError ("in order to access the field" <+> pPrint fieldId <+> ", expected" <+> pPrint tm <+> " to have a structure type") (pure . toSyntax $ term)
    ty <- case fieldId `lookup` structureFields struct of
      Nothing -> throwTypingError ("attempted to access the field" <+> pPrint fieldId <+> ", but it has structure type" <+> pPrint (structureId struct) <+> " which does not have that field") (pure . toSyntax $ term)
      Just ty -> return (normType ty)
    return $ TermMember tm' fieldId ty
  TermNeutral app mb_args mb_cxargs () -> synthNeutral term app mb_args mb_cxargs
  TermAscribe tm ty () -> synthCheckTerm' (normType ty) tm
  TermMatch tm branches () -> synthMatch tm branches

synthPrimitive :: Term () -> Primitive () -> TypingM (Term TypeM)
synthPrimitive _term prim = case prim of
  PrimitiveTry tm -> do
    -- synth type of arg, alpha
    tm' <- synthTerm tm
    -- output type is Optional alpha
    ty <- TypeOptional <$$> inferTerm tm'
    return $ TermPrimitive (PrimitiveTry tm') ty
  PrimitiveCast tm -> do
    -- synth type of arg, alpha
    tm' <- synthTerm tm
    -- output type is a fresh type unification var, with constraint that it
    -- was casted from an alpha
    ty <- freshTypeUnfiyVar "cast" . pure . UnifyConstraintCasted =<<< inferTerm tm'
    -- sets the top term ann to be the result type of casting, which unwraps
    -- the cast; unwraps the cast
    return $ mapTopAnnTerm (const ty) tm'
  PrimitiveTuple tms -> do
    -- synth type of each term, alphas
    tms' <- synthTerm `traverse` tms
    -- output type is Tuple alphas
    tys <- inferTerm `traverse` tms'
    return $ TermPrimitive (PrimitiveTuple tms') (TypeTuple <$> sequence tys)
  -- TODO: actually, just iterate through array after introducing new type
  -- var, and make sure all unfiy with it
  PrimitiveArray tms -> do
    -- array elem type as unification var
    ty <- freshTypeUnfiyVar "primitive array" Nothing
    -- unify each elem of array with array elem type var, which will
    -- substitute it appropriately
    tms' <- synthCheckTerm' ty `traverse` tms
    return $ TermPrimitive (PrimitiveArray tms') (TypeArray <$> ty)
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
  PrimitiveEq tm1 tm2 -> do
    tm1' <- synthTerm tm1
    ty <- inferTerm tm1'
    tm2' <- synthCheckTerm' ty tm2
    return $ TermPrimitive (PrimitiveEq tm1' tm2') (normType TypeBit)
  PrimitiveAdd tm1 tm2 -> do
    tyM <- freshTypeUnfiyVar "add" (Just UnifyConstraintNumeric)
    tm1' <- synthCheckTerm' tyM tm1
    tm2' <- synthCheckTerm' tyM tm2
    join $ unify' <$> inferTerm tm1' <*> inferTerm tm2'
    return $ TermPrimitive (PrimitiveAdd tm1' tm2') tyM

synthNeutral :: Term () -> Applicant () -> Maybe [Term ()] -> Maybe [Term ()] -> TypingM (Term TypeM)
-- function application / newtype constructor
-- newtype constructor
-- variant constructor
-- enum constructor
synthNeutral term app mb_args mb_cxargs =
  lookupApplicant app >>= \app' -> case applicantAnn app' of
    -- TypeFunction
    ApplicantTypeFunction FunctionType {..} -> do
      args <- case mb_args of
        Nothing -> throwTypingError "a function application must have arguments" (pure . toSyntax $ term)
        Just args -> return args
      -- check args
      -- mb_args' <- pure <$> uncurry synthCheckTerm `traverse` ((snd <$> functionTypeParameters func) `zip` args)
      mb_args' <- Just <$> forM ((snd <$> functionTypeParameters) `zip` args) (uncurry synthCheckTerm')
      -- check cxargs
      mb_cxargs' <-
        case mb_cxargs of
          -- implicitly give, or no cxargs
          Nothing -> do
            case functionTypeContextualParameters of
              -- no cxparams
              Nothing -> return Nothing
              -- implicit cxparams
              Just cxparams ->
                fmap Just . forM cxparams $ \(tyIdCxparam, tmIdCxparam) ->
                  asks (^. ctxCxparams . at tyIdCxparam) >>= \case
                    Nothing -> throwTypingError ("could not infer the implicit contextual argument" <+> ticks (pPrint tmIdCxparam)) (pure . toSyntax $ term)
                    Just tmIdCxarg ->
                      return $
                        TermNeutral
                          Applicant
                            { applicantMaybeTypeId = Nothing,
                              applicantTermId = tmIdCxarg,
                              applicantAnn = ApplicantType $ normType $ TypeNamed tyIdCxparam
                            }
                          Nothing
                          Nothing
                          (normType $ TypeNamed tyIdCxparam)
          Just cxargs -> do
            case functionTypeContextualParameters of
              -- actually, no cxparams!
              Nothing -> throwTypingError "attempted to give explicit contextual arguments to a function that does not have contextual parameters" (pure . toSyntax $ term)
              Just cxparams -> do
                cxargs1 <- synthTerm `traverse` cxargs
                newtyIds_cxargs <- forM cxargs1 \tm ->
                  inferTerm tm >>>= \case
                    TypeNewtype newty -> return (newtypeId newty, tm)
                    _ -> throwTypingError "each explicit contextual argument must be a newtype" (pure . toSyntax $ term)
                -- cxargs' is in the same order as cxparams
                cxargs' <-
                  reverse . snd
                    <$> foldM
                      ( \(newtyIds_cxargs1, cxargs') newtyId -> do
                          -- extract the cxarg that has the right newtype
                          case newtyId `lookup` newtyIds_cxargs1 of
                            Nothing -> throwTypingError ("missing explicit contextual argument: " <+> pPrint newtyId) (pure . toSyntax $ term)
                            Just cxarg -> return (newtyIds_cxargs, cxarg : cxargs')
                      )
                      (newtyIds_cxargs, [])
                      (fst <$> cxparams)
                return $ pure cxargs'
      return $ TermNeutral app' mb_args' mb_cxargs' functionTypeOutput
    -- TypeVariantConstuctor
    ApplicantTypeVariantConstructor varntM _constrId tyParamsM -> do
      -- check arguments
      mb_args' <-
        case mb_args of
          Nothing -> throwTypingError "the variant constructor expects some arguments, but none were given" (pure . toSyntax $ term)
          Just args -> do
            tyParams <- sequence tyParamsM
            pure <$> uncurry synthCheckTerm `traverse` (tyParams `zip` args)
      -- check contextual args (can't have any)
      unless (isNothing mb_cxargs) $ throwTypingError "a variant constructor can't have contextual arguments" (pure . toSyntax $ term)
      -- output type
      let tyOut = normType . TypeVariant =<< sequence varntM
      return $ TermNeutral app' mb_args' Nothing tyOut
    -- TypeEnumConstructor
    ApplicantTypeEnumConstructor enum _constrId -> do
      -- check arguments (can't have any)
      unless (isNothing mb_args) $ throwTypingError "cannot apply an enum constructor" (pure . toSyntax $ term)
      -- check contextual args (can't have any)
      unless (isNothing mb_cxargs) $ throwTypingError "cannot give contextual argsuments to an enum constructor" (pure . toSyntax $ term)
      -- output type
      let tyOut = normType . TypeEnum =<< sequence enum
      return $ TermNeutral app' Nothing Nothing tyOut
    -- TypeNewtypeConstructor
    ApplicantTypeNewtypeConstructor newtyM -> do
      newty <- sequence newtyM
      -- check argument (must have exactly 1)
      mb_args' <- case mb_args of
        Just [arg] ->
          pure . pure <$> synthCheckTerm (newtypeType newty) arg
        Just _ -> throwTypingError "a newtype constructor requires exactly one argument" (pure . toSyntax $ term)
        Nothing -> throwTypingError "a newtype constructor must be given an argument" (pure . toSyntax $ term)
      unless (isNothing mb_cxargs) $ throwTypingError "a newtype constructor cannot be given contextual arguments" (pure . toSyntax $ term)
      -- output type
      let tyOut = normType $ TypeNewtype newty
      return $ TermNeutral app' mb_args' Nothing tyOut
    ApplicantType tyM -> do
      -- check arguments (must have 0)
      -- check contextual arguments (must have 0)
      unless (isNothing mb_args) $ throwTypingError "cannot apply a constant" (pure . toSyntax $ term)
      unless (isNothing mb_args) $ throwTypingError "cannot give contextual arguments to a constant" (pure . toSyntax $ term)
      return $ TermNeutral app' Nothing Nothing tyM

synthMatch :: Term () -> Branches () -> TypingM (Term TypeM)
synthMatch arg branches = do
  arg' <- synthTerm arg
  tyArg <- inferTerm arg'
  tyBody <- freshTypeUnfiyVar "match body" Nothing
  branches' <- forM branches \(pat, body) -> do
    pat' <- checkPattern' tyArg pat
    body' <- synthCheckTerm' tyBody body
    return (pat', body')
  return $ TermMatch arg' branches' tyBody

synthRefinement :: Refinement () -> TypingM (Refinement TypeM)
synthRefinement (Refinement tm) = Refinement <$> synthCheckTerm TypeBit tm

-- ** Normalization

normType :: Type -> TypeM
normType type_ = case type_ of
  TypeArray ty -> TypeArray <$> normType ty
  TypeTuple tys -> TypeTuple <$> normType `traverse` tys
  TypeOptional ty -> TypeOptional <$> normType ty
  TypeNamed tyId -> join $ lookupType tyId
  TypeUnifyVar uv _ ->
    gets (^. envUnification . at uv) >>= \case
      Nothing -> return type_
      Just ty -> normType ty
  TypeStructure struct -> do
    structureFields <- secondM normType `traverse` structureFields struct
    return $ TypeStructure struct {structureFields}
  TypeVariant varnt -> do
    variantConstructors <- secondM (traverse normType) `traverse` variantConstructors varnt
    return $ TypeVariant varnt {variantConstructors}
  TypeNewtype newty -> do
    newtypeType <- normType (newtypeType newty)
    return $ TypeNewtype newty {newtypeType}
  -- TypeFunction fun -> do
  --   functionTypeParameters <- secondM normType `traverse` functionTypeParameters fun
  --   functionTypeOutput <- normType $ functionTypeOutput fun
  --   return $ TypeFunction fun {functionTypeParameters, functionTypeOutput}
  -- TypeVariantConstuctor varnt constrId mb_args -> do
  --   variantConstructors <- secondM (traverse (traverse normType)) `traverse` variantConstructors varnt
  --   mb_args' <- traverse (traverse normType) mb_args
  --   return $ TypeVariantConstuctor varnt {variantConstructors} constrId mb_args'
  -- TypeNewtypeConstructor newty -> do
  --   newtypeType <- normType (newtypeType newty)
  --   return $ TypeNewtypeConstructor newty {newtypeType}
  _ -> return type_

-- ** TypeUnifVar

freshTypeUnfiyVar :: String -> Maybe UnifyConstraint -> TypingM TypeM
freshTypeUnfiyVar str mb_uc = do
  i <- gets (^. envFreshUnificationVarIndex)
  modifying envFreshUnificationVarIndex (+ 1)
  return $ normType (TypeUnifyVar (UnifyVar str i) mb_uc)

-- ** Unification

unify' :: TypingM Type -> TypingM Type -> TypingM ()
unify' tyMExpect tyMSynth = do
  ty1 <- tyMExpect
  ty2 <- tyMSynth
  unify ty1 ty2

unifyVarOccursInType :: UnifyVar -> Type -> Bool
unifyVarOccursInType uv = \case
  TypeArray ty -> unifyVarOccursInType uv ty
  TypeTuple tys -> any (unifyVarOccursInType uv) tys
  TypeOptional ty -> unifyVarOccursInType uv ty
  TypeUnifyVar uv' _ | uv == uv' -> True
  _ -> False

satisfiesUnifyConstraint :: Type -> UnifyConstraint -> Bool
satisfiesUnifyConstraint ty = \case
  UnifyConstraintCasted ty' -> case (ty, ty') of
    (TypeNumber numty1 _size1, TypeNumber numty2 _size2)
      | all (`elem` [TypeInt, TypeUInt]) [numty1, numty2] -> True
      | all (`elem` [TypeFloat]) [numty1, numty2] -> True
    (TypeUnifyVar _ mb_uc, _) -> maybe True (satisfiesUnifyConstraint ty') mb_uc
    -- FlexBug.throw $ FlexLog "typing" $ "this case of `satisfiesUnifyConstraint` is not implemented yet:" $+$ nest 4 ("type =" <+> pPrint ty) $+$ nest 4 ("unifyConstraint =" <+> pPrint uc)
    _uc -> False
  UnifyConstraintNumeric -> case ty of
    TypeNumber _ _ -> True
    _ -> False

-- | <expected type> ~? <synthesized type>
unify :: Type -> Type -> TypingM ()
-- TypeUnifyVar
unify ty1@(TypeUnifyVar uv mb_uc) ty2@ty = substUnifyVar ty1 ty2 uv mb_uc ty
unify ty1@ty ty2@(TypeUnifyVar uv mb_uc) = substUnifyVar ty1 ty2 uv mb_uc ty
-- simple types
unify ty1@(TypeNumber numty1 size1) ty2@(TypeNumber numty2 size2) = unless (numty1 == numty2 && size1 == size2) $ throwUnifyError ty1 ty2 Nothing
unify TypeBit TypeBit = return ()
unify TypeChar TypeChar = return ()
-- complex types
unify (TypeArray ty1) (TypeArray ty2) = unify ty1 ty2
unify (TypeTuple tys1) (TypeTuple tys2) = uncurry unify `traverse_` (tys1 `zip` tys2)
unify (TypeOptional ty1) (TypeOptional ty2) = unify ty1 ty2
unify (TypeStructure struct1) (TypeStructure struct2) | structureId struct1 == structureId struct2 = return ()
unify (TypeEnum enum1) (TypeEnum enum2) | enumId enum1 == enumId enum2 = return ()
unify (TypeVariant varnt1) (TypeVariant varnt2) | variantId varnt1 == variantId varnt2 = return ()
unify (TypeNewtype newty1) (TypeNewtype newty2) | newtypeId newty1 == newtypeId newty2 = return ()
-- invalid types
-- TODO: remove when confirm deletions of Type*Constructor types
-- unify ty@(TypeFunction {}) _ = FlexBug.throw $ FlexLog "typing" $ "should never try to unify with `TypeFunction`:" <+> ticks (pPrint ty)
-- unify _ ty@(TypeFunction {}) = FlexBug.throw $ FlexLog "typing" $ "should never try to unify with `TypeFunction`:" <+> ticks (pPrint ty)
-- unify ty@TypeVariantConstuctor {} _ = FlexBug.throw $ FlexLog "typing" $ "should never try to unify with `TypeVariantConstructor`:" <+> ticks (pPrint ty)
-- unify _ ty@TypeVariantConstuctor {} = FlexBug.throw $ FlexLog "typing" $ "should never try to unify with `TypeVariantConstructor`:" <+> ticks (pPrint ty)
-- unify ty@TypeEnumConstructor {} _ = FlexBug.throw $ FlexLog "typing" $ "should never try to unify with `TypeEnumConstructor`:" <+> ticks (pPrint ty)
-- unify _ ty@TypeEnumConstructor {} = FlexBug.throw $ FlexLog "typing" $ "should never try to unify with `TypeEnumConstructor`:" <+> ticks (pPrint ty)
-- unify ty@TypeNewtypeConstructor {} _ = FlexBug.throw $ FlexLog "typing" $ "should never try to unify with `TypeNewtypeConstructor`:" <+> ticks (pPrint ty)
-- unify _ ty@TypeNewtypeConstructor {} = FlexBug.throw $ FlexLog "typing" $ "should never try to unify with `TypeNewtypeConstructor`:" <+> ticks (pPrint ty)
unify ty@TypeNamed {} _ = FlexBug.throw $ FlexLog "typing" $ "`TypeNamed` should never appear in a normalized type:" <+> ticks (pPrint ty)
unify _ ty@TypeNamed {} = FlexBug.throw $ FlexLog "typing" $ "`TypeNamed` should never appear in a normalized type:" <+> ticks (pPrint ty)
-- non-unifiable types
unify tyExpect tySynth = throwUnifyError tyExpect tySynth Nothing

substUnifyVar :: Type -> Type -> UnifyVar -> Maybe UnifyConstraint -> Type -> TypingM ()
substUnifyVar ty1 ty2 uv mb_uc ty = do
  -- check if uv1 occurs in ty2
  when (uv `unifyVarOccursInType` ty) $ throwUnifyError ty1 ty2 (Just "fails occurs check")
  case mb_uc of
    Nothing -> return ()
    -- check if ty satisfies the constraints uc
    Just uc -> unless (ty `satisfiesUnifyConstraint` uc) $ throwUnifyError ty1 ty2 (Just $ "it does not satisfy unification constraint:" <+> pPrint uc)
  modifying (envUnification . at uv) \case
    Just ty' -> FlexBug.throw $ FlexLog "typing" $ "trying to substitute" <+> ticks (pPrint uv) <+> "for" <+> ticks (pPrint ty) <+> ", but it's already be substituted for" <+> pPrint ty'
    Nothing -> Just ty

throwUnifyError :: Type -> Type -> Maybe Doc -> TypingM a
throwUnifyError tyExpect tySynth mb_msg =
  throwTypingError
    ( "failed to unify synthesized type"
        $+$ nest 2 (pPrint tySynth)
        $$ "with expected type"
        $+$ nest 2 (pPrint tyExpect)
        $$ maybe mempty ("because" <+>) mb_msg
    )
    Nothing

-- ** Inference

inferTerm :: Term TypeM -> TypingM TypeM
inferTerm =
  return . \case
    TermLiteral _ ty -> normType =<< ty
    TermLet _ _ _ ty -> normType =<< ty
    TermAssert _ _ ty -> normType =<< ty
    TermStructure _ _ ty -> normType =<< ty
    TermMember _ _ ty -> normType =<< ty
    TermAscribe _ _ ty -> normType =<< ty
    TermMatch _ _ ty -> normType =<< ty
    TermNeutral _ _ _ ty -> normType =<< ty
    TermPrimitive _ ty -> normType =<< ty

-- get the type of a literal
typeLiteral :: Literal -> TypingM TypeM
typeLiteral = \case
  LiteralInteger _ -> freshTypeUnfiyVar "literal integer" . pure $ UnifyConstraintCasted $ TypeNumber TypeInt 32
  LiteralFloat _ -> freshTypeUnfiyVar "literal float" . pure $ UnifyConstraintCasted $ TypeNumber TypeFloat 64
  LiteralBit _ -> return . normType $ TypeBit
  LiteralChar _ -> return . normType $ TypeChar
  LiteralString _ -> return . normType $ TypeArray TypeChar

-- ** Specification of Typing

-- | After typing, the valid syntax for types and terms is more specific. In
-- particular, types must be normal and a few term constructors are unwrapped.
-- These properties are specified by the following predicates.

-- | Forms that are not allowed in normal types are:
-- - TypeNamed
-- - TypeUnifyVar that has already been substited by current unifying
--   substitution
isNormalType :: Type -> TypingM Bool
isNormalType = \case
  TypeNumber _nt _n -> return True
  TypeBit -> return True
  TypeChar -> return True
  TypeArray ty -> isNormalType ty
  TypeTuple tys -> and <$> isNormalType `traverse` tys
  TypeOptional ty -> isNormalType ty
  TypeNamed {} -> return False
  TypeUnifyVar uv _ ->
    -- a unification variable is only normal if it hasn't been substituted yet
    gets (^. to _envUnification . at uv) >>= \case
      Nothing -> return True
      Just _ -> return False
  TypeStructure {} -> return True
  TypeEnum {} -> return True
  TypeVariant {} -> return True
  TypeNewtype {} -> return True

-- TypeFunction {} -> return True
-- TypeVariantConstuctor {} -> return True
-- TypeEnumConstructor {} -> return True
-- TypeNewtypeConstructor {} -> return True

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
    PrimitiveEq te te' -> isTypedTerm `all` [te, te']
    PrimitiveAdd te te' -> isTypedTerm `all` [te, te']
  TermLet _ tm bod _ -> isTypedTerm `all` [tm, bod]
  TermAssert tm bod _ -> isTypedTerm `all` [tm, bod]
  TermStructure _ fields _ -> isTypedTerm `all` (snd <$> fields)
  TermMember te _ _ -> isTypedTerm te
  TermNeutral _app m_tes m_te's _ -> maybe True (isTypedTerm `all`) m_tes && maybe True (isTypedTerm `all`) m_te's
  TermAscribe {} -> False
  TermMatch te branches _ -> isTypedTerm te && isTypedTerm `all` (snd <$> branches)

assertNormalModule :: Module Type -> TypingM ()
assertNormalModule =
  void . traverse
    \ty ->
      unlessM (isNormalType ty) $
        throwTypingError ("type in module is not normal:" <+> pPrint ty) Nothing

isTypedModule :: Module Type -> Bool
isTypedModule Module {..} =
  all
    ( \de -> case de of
        (DeclarationStructure {}) -> True
        (DeclarationNewtype {}) -> True
        (DeclarationVariant {}) -> True
        (DeclarationEnum {}) -> True
        (DeclarationAlias {}) -> True
        (DeclarationFunction Function {..}) -> isTypedTerm functionBody
        (DeclarationConstant Constant {..}) -> isTypedTerm constantBody
        (DeclarationRefinedType {}) -> True
    )
    moduleDeclarations

defaultType :: Type -> TypingM Type
defaultType type_ = case type_ of
  TypeArray ty -> TypeArray <$> defaultType ty
  TypeTuple tys -> TypeTuple <$> defaultType `traverse` tys
  TypeOptional ty -> TypeOptional <$> defaultType ty
  TypeUnifyVar _uv m_uc -> case m_uc of
    Nothing -> throwTypingError ("can't default unconstrained unification type variable:" <+> pPrint type_) Nothing
    Just uc -> case uc of
      UnifyConstraintCasted ty -> return ty
      -- TODO: maybe you want a different default number type?
      UnifyConstraintNumeric -> return $ TypeNumber TypeInt 32
  TypeStructure struct@Structure {..} -> do
    fields <- secondM defaultType `traverse` structureFields
    return $ TypeStructure struct {structureFields = fields}
  TypeVariant varnt@Variant {..} -> do
    constrs <- secondM (defaultType `traverse`) `traverse` variantConstructors
    return $ TypeVariant varnt {variantConstructors = constrs}
  TypeNewtype newty@Newtype {..} -> do
    ty <- defaultType newtypeType
    return $ TypeNewtype newty {newtypeType = ty}
  ty -> return ty
