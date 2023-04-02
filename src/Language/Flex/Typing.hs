module Language.Flex.Typing where

import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (WriterT (WriterT, runWriterT), tell)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Language.Flex.FlexM (MonadFlex)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Syntax as Syntax
import Language.Flex.Typing.Module
import Language.Flex.Typing.TypingM
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum, enum)

-- ** Typing

-- typeModule :: (MonadFlex m, ModuleError TypingError m, TypingError m) => Module Type () -> m (Module Type Type)
-- m (Either TypingError (Module Type Type, TypingEnv))
typeModule :: (MonadError TypingError m, MonadFlex m) => Module Type () -> m (TypingEnv, Module Type Type)
typeModule mdl = FlexM.markSection [FlexM.FlexMarkStep "typeModule" . Just $ pPrint mdl] do
  ctx <- moduleTypingCtx mdl
  env <- moduleTypingEnv mdl
  (env', mdl') <- runTypingM' env ctx do
    mdl' <- synthModule mdl
    -- make sure to synthModule before getting state
    env' <- get
    return (env', mdl')
  return (env', mdl')

-- ** Synthesizing

-- | Type annotations are defaulted and normalized.
synthModule :: Module Type () -> TypingM (Module Type Type)
synthModule mdl@Module {..} = do
  moduleDeclarations' <- forM moduleDeclarations synthDeclaration
  return mdl {moduleDeclarations = moduleDeclarations'}

synthDeclaration :: Declaration Type () -> TypingM (Declaration Type Type)
synthDeclaration = \case
  (DeclarationFunction fun) -> DeclarationFunction <$> synthFunction fun
  (DeclarationConstant con) -> DeclarationConstant <$> synthConstant con
  (DeclarationRefinedType rt) -> DeclarationRefinedType <$> synthRefinedType rt
  (DeclarationStructure struct) -> return $ DeclarationStructure struct
  (DeclarationNewtype newty) -> return $ DeclarationNewtype newty
  (DeclarationVariant varnt) -> return $ DeclarationVariant varnt
  (DeclarationEnum enum) -> return $ DeclarationEnum enum
  (DeclarationAlias alias) -> return $ DeclarationAlias alias

synthFunction :: Function Type () -> TypingM (Function Type Type)
synthFunction fun@Function {..} = do
  let FunctionType {..} = functionType
  let intros :: TypingCtx -> TypingM TypingCtx
      intros = execStateT do
        -- intro params
        forM_ functionParameters \(paramId, paramType) -> do
          -- assert param type is normal
          paramType' <- lift . return $ assertNormalType paramType
          let app =
                Applicant
                  { applicantMaybeTypeId = Nothing,
                    applicantTermId = paramId,
                    applicantAnn = ApplicantType paramType'
                  }
          -- assert no shadowing
          assertNoShadowing app (SyntaxDeclaration $ DeclarationFunction fun)
          -- add applicant
          modifyInsertUnique (ctxApplicants . at (void app)) app $ TypingError ("attempted to shadow" <+> ticks (pPrint (void app))) (Just (SyntaxDeclaration $ DeclarationFunction fun))

        -- intro contextual params
        forM_ functionContextualParameters $ mapM \(cxparamNewtypeId, cxparamId) -> do
          cxparamType' <- lift . return $ assertNormalType $ TypeNamed cxparamNewtypeId
          let app =
                Applicant
                  { applicantMaybeTypeId = Nothing,
                    applicantTermId = cxparamId,
                    applicantAnn = ApplicantType cxparamType'
                  }
          -- add applicant
          modifyInsertUnique (ctxApplicants . at (void app)) app $ TypingError ("attempted to shadow" <+> ticks (pPrint (void app))) (Just (SyntaxDeclaration $ DeclarationFunction fun))
  localM intros do
    functionBody1 <- synthCheckTerm' (normalizeType functionOutput) functionBody
    functionBody2 <- normalizeInternalTypes functionBody1
    functionBody3 <- defaultInternalTypes functionBody2
    return fun {functionBody = functionBody3}
  where
    assertNoShadowing app syn = do
      gets (^. ctxApplicants . at (void app)) >>= \case
        Just app' -> throwTypingError ("attempted to introduce" <+> ticks (pPrint (void app)) <+> "which shadows" <+> ticks (pPrint (void app'))) (Just syn)
        Nothing -> return ()

synthConstant :: Constant Type () -> TypingM (Constant Type Type)
synthConstant con@Constant {..} = do
  constantBody1 <- synthCheckTerm' (normalizeType constantType) constantBody
  constantBody2 <- normalizeInternalTypes constantBody1
  constantBody3 <- defaultInternalTypes constantBody2
  return con {constantBody = constantBody3}

synthRefinedType :: RefinedType () -> TypingM (RefinedType Type)
synthRefinedType rt@(RefinedType {..}) = do
  refinedTypeRefinement1 <- synthRefinement refinedTypeRefinement
  refinedTypeRefinement2 <- normalizeInternalTypes refinedTypeRefinement1
  refinedTypeRefinement3 <- defaultInternalTypes refinedTypeRefinement2
  return rt {refinedTypeRefinement = refinedTypeRefinement3}

synthRefinement :: Refinement () -> TypingM (Refinement MType)
synthRefinement (Refinement tm) = Refinement <$> synthCheckTerm TypeBit tm

synthTerm :: Term () -> TypingM (Term MType)
synthTerm (TermLiteral lit ()) = TermLiteral lit <$> synthLiteral lit
synthTerm (TermPrimitive prim ()) = synthPrimitive prim
synthTerm (TermLet pat te1 te2 ()) = do
  te1' <- synthTerm te1
  pat' <- synthCheckPattern' (termAnn te1') pat
  introPattern pat' $ synthTerm te2
synthTerm (TermAssert tm1 tm2 ()) = do
  tm1' <- synthCheckTerm TypeBit tm1
  tm2' <- synthTerm tm2
  return $ TermAssert tm1' tm2' (termAnn tm2')
synthTerm term0@(TermStructure structId fields ()) =
  lookupType structId >>= \case
    (CtxStructure Structure {..}) -> do
      -- put fields into canonical order as determined by structure
      fieldItems <- case zipAssoc structureFields fields of
        Left (missing, extra, dups) -> throwTypingError ("the structure constructor is missing fields" <+> pPrint (fst <$> missing) <+> ", has extra fields" <+> pPrint (fst <$> extra) <+> ", and has duplicated fields" <+> pPrint (fst <$> dups)) (Just $ toSyntax term0)
        Right fieldItems -> return fieldItems
      -- synth-check fields
      fields' <- forM fieldItems \(fieldId, (fieldMType, fieldTerm)) -> do
        fieldTerm' <- synthCheckTerm' fieldMType fieldTerm
        return (fieldId, fieldTerm')
      let structType = return $ TypeNamed structId
      return $ TermStructure structId fields' structType
    ctxType -> throwTypingError ("the type id" <+> ticks (pPrint structId) <+> "was expected to be of a struct, but it is actually of" <+> ticks (pPrintDeclarationHeader (fromCtxTypeToDeclaration ctxType))) (Just $ toSyntax term0)
synthTerm term0@(TermMember tm fieldId ()) = do
  tm' <- synthTerm tm
  termAnn tm' >>= \case
    (TypeNamed tyId) ->
      lookupType tyId >>= \case
        (CtxStructure Structure {..}) -> do
          fieldMType <- case fieldId `lookup` structureFields of
            Nothing -> throwTypingError ("the structure" <+> ticks (pPrint structureId) <+> "does not have the field" <+> ticks (pPrint fieldId)) (Just $ toSyntax term0)
            Just fieldMType -> return fieldMType
          return $ TermMember tm' fieldId fieldMType
        ctxType -> throwTypingError ("the base of the field access has non-structure type" <+> ticks (pPrint ctxType)) (Just $ toSyntax term0)
    type_ -> throwTypingError ("the base of the field access has non-structure type" <+> ticks (pPrint type_)) (Just $ toSyntax term0)
synthTerm term0@(TermNeutral {..}) = do
  let mb_syn = Just $ toSyntax term0
  app'@Applicant {..} <- lookupApplicant termApplicant
  case applicantAnn of
    (ApplicantTypeFunction funId) -> do
      FunctionType {..} <- lookupFunctionType funId

      -- assert that correct number of args are given
      args <- case termMaybeArgs of
        Nothing -> throwTypingError "function application requires a list of arguments" mb_syn
        Just args -> return args
      unless (length termMaybeArgs == length functionParameters) $ throwTypingError "incorrect number of arguments given to function application" mb_syn

      -- synth-check args
      args' <- forM (functionParameters `zip` args) \((_argId, argMType), argTerm) -> synthCheckTerm' argMType argTerm

      mb_cxargs' <- case functionContextualParameters of
        Nothing -> do
          -- assert that no contextual args are given
          unless (isNothing termMaybeCxargs) $ throwTypingError "function application provided explicit contextual arguments when the function does not have contextual parameters" mb_syn
          return Nothing
        Just cxparams ->
          do
            case termMaybeCxargs of
              -- infer implicit contextual arguments
              Nothing -> do
                let cxargsWriter :: WriterT [(TermId, TypeId)] TypingM [Maybe (TermId, TypeId)]
                    cxargsWriter = forM cxparams \(cxparamNewtypeId, cxparamId) -> do
                      -- look in context for a cxarg that has the right newtype id
                      -- lookup in context a cxparam that has the right newtype id
                      asks (^. ctxCxparamIds . at cxparamNewtypeId) >>= \case
                        Nothing -> do
                          tell [(cxparamId, cxparamNewtypeId)]
                          return Nothing
                        Just cxparamId' -> return $ Just (cxparamId', cxparamNewtypeId)
                cxargs <-
                  runWriterT cxargsWriter >>= \(ls_mb_cxarg, missing) ->
                    if not (null missing)
                      then do
                        -- there are some cxargs missing
                        throwTypingError ("could not infer the implicit contextual arguments:" <+> commaList (missing <&> \(cxparamId, cxparamNewtypeId) -> pPrint cxparamId <+> ":" <+> pPrint cxparamNewtypeId)) mb_syn
                      else do
                        -- all cxargs were inferred
                        return . flip concatMap ls_mb_cxarg $
                          maybe [] . comp1 pure $ \(cxargId, cxNewtypeId) ->
                            let cxargMType = normalizeType $ TypeNamed cxNewtypeId
                             in TermNeutral
                                  Applicant
                                    { applicantMaybeTypeId = Nothing,
                                      applicantTermId = cxargId,
                                      applicantAnn = ApplicantType cxargMType
                                    }
                                  Nothing
                                  Nothing
                                  cxargMType

                return $ Just cxargs
              -- check explicit contextual args
              Just cxargs -> do
                -- synth and check that each cxarg has a newtype type
                cxargs' <- forM cxargs \cxarg -> do
                  cxarg' <- synthTerm cxarg
                  Syntax.termAnn cxarg' >>= \case
                    TypeNamed tyId ->
                      lookupType tyId >>= \case
                        (CtxNewtype Newtype {..}) -> return (newtypeId, cxarg')
                        ctxType -> throwTypingError ("function call's explicit contextual argument" <+> ticks (pPrint cxarg') <+> "has a non-newtype type" <+> ticks (pPrint ctxType)) mb_syn
                    type_ -> throwTypingError ("function call's explicit contextual argument" <+> ticks (pPrint cxarg') <+> "has a non-newtype type" <+> ticks (pPrint type_)) mb_syn
                -- put contextual args in canonical order
                cxargs'' <- case zipAssoc cxparams cxargs' of
                  Left (missing, extra, dups) -> throwTypingError ("function call's explicit contextual arguments is missing contextual arguments" <+> commaList (missing <&> \(newtypeId, paramId) -> pPrint paramId <+> ":" <+> pPrint newtypeId) <+> ", has extra contextual arguments with newtypes" <+> commaList (pPrint <$> extra) <+> "and has contextual arguments with overlapping newtypes for newtypes" <+> commaList (pPrint . fst <$> dups)) mb_syn
                  Right cxargsItemsAssoc -> return $ snd . snd <$> cxargsItemsAssoc
                return $ Just cxargs''
      return
        TermNeutral
          { termApplicant = app',
            termMaybeArgs = Just args',
            termMaybeCxargs = mb_cxargs',
            termAnn = functionOutput
          }
    (ApplicantTypeEnumConstructor enumId _ctorId) ->
      lookupType enumId
        >>= \case
          (CtxEnum Enum {..}) -> do
            -- assert no args
            unless (isNothing mb_args) $ throwTypingError "an enum construction cannot have arguments" mb_syn
            -- assert no cxargs
            unless (isNothing termMaybeCxargs) $ throwTypingError "an enum construction cannot have contextual arguments" mb_syn
            return $ TermNeutral app' Nothing Nothing (return $ TypeNamed enumId)
          ctxType -> FlexM.throw $ "the context stores that the applicant is an enum constructor," <+> ticks (pPrint app') <+> "but the type corresponding to the type id is not an enum," <+> ticks (pPrint ctxType)
    (ApplicantTypeVariantConstructor varntId ctorId) -> do
      lookupType varntId >>= \case
        CtxVariant Variant {..} -> do
          -- assert no cxargs
          unless (isNothing termMaybeCxargs) $ throwTypingError "a variant construction cannot have contextual arguments" mb_syn
          -- synth-check args
          args <- case mb_args of
            Nothing -> throwTypingError "a variant construction must have arguments" mb_syn
            Just args -> return args
          params <- case ctorId `lookup` variantConstructors of
            Nothing -> throwTypingError "the variant doesn't have the constructor" mb_syn
            Just params -> return params
          args' <- forM (params `zip` args) (uncurry synthCheckTerm')
          return $ TermNeutral app' (Just args') Nothing (return $ TypeNamed variantId)
        ctxType -> FlexM.throw $ "the context stores that the applicant is a variant constructor," <+> ticks (pPrint app') <+> "but the type corresponding to the type id is not a variant," <+> ticks (pPrint ctxType)
    (ApplicantTypeNewtypeConstructor newtyId) -> do
      lookupType newtyId >>= \case
        CtxNewtype Newtype {..} -> do
          -- assert no cxarsg
          unless (isNothing termMaybeCxargs) $ throwTypingError "a newtype construction cannot have contextual arguments" mb_syn
          -- assert single arg
          arg <- case mb_args of
            Just [arg] -> return arg
            _ -> throwTypingError "a newtype construction must have exactly one argument" mb_syn
          -- synth-check arg
          arg' <- synthCheckTerm' newtypeType arg
          return $ TermNeutral app' (Just [arg']) Nothing (return $ TypeNamed newtypeId)
        ctxType -> FlexM.throw $ "the context stores that the applicant is a newtype constructor," <+> ticks (pPrint app') <+> "but the type corresponding to the type id is not a newtype," <+> ticks (pPrint ctxType)
    (ApplicantType mtype) -> do
      -- assert no args
      unless (isNothing mb_args) $ throwTypingError "arguments given to a non-function" mb_syn
      -- assert no cxargs
      unless (isNothing termMaybeCxargs) $ throwTypingError "explicit contextual arguments given to a non-function" mb_syn
      return $ TermNeutral app' Nothing Nothing mtype
synthTerm (TermAscribe tm ty ()) =
  synthCheckTerm' (normalizeType ty) tm
synthTerm term0@(TermMatch tm branches ()) = do
  tm' <- synthTerm tm
  ty <- freshTypeUnifyVar' (render $ "match result" <+> pPrint term0) Nothing
  branches' <- forM branches (synthCheckBranch (termAnn tm') ty)
  return $ TermMatch tm' branches' ty

synthCheckBranch :: MType -> MType -> Branch () -> TypingM (Branch MType)
synthCheckBranch inMType outMType (pat, tm) = do
  pat' <- synthCheckPattern' inMType pat
  tm' <- introPattern pat' $ synthCheckTerm' outMType tm
  return (pat', tm')

synthLiteral :: Literal -> TypingM MType
synthLiteral (LiteralInteger n) = freshTypeUnifyVar' (render $ "literal integer" <+> pPrint n) (pure $ UnifyConstraintCasted (TypeNumber TypeInt 32))
synthLiteral (LiteralFloat x) = freshTypeUnifyVar' (render $ "literal float" <+> pPrint x) (pure $ UnifyConstraintCasted (TypeNumber TypeFloat 32))
synthLiteral (LiteralBit b) = freshTypeUnifyVar' (render $ "literal bit" <+> pPrint b) (pure $ UnifyConstraintCasted TypeBit)
synthLiteral (LiteralChar c) = freshTypeUnifyVar' (render $ "literal char" <+> pPrint c) (pure $ UnifyConstraintCasted TypeChar)
synthLiteral (LiteralString s) = freshTypeUnifyVar' (render $ "literal string" <+> pPrint s) (pure $ UnifyConstraintCasted (TypeArray TypeChar))

synthPrimitive :: Primitive () -> TypingM (Term MType)
synthPrimitive (PrimitiveTry te) = do
  te' <- synthTerm te
  return $ TermPrimitive (PrimitiveTry te') (TypeOptional <$> termAnn te')
synthPrimitive (PrimitiveCast te) = do
  te' <- synthTerm te
  -- since we haven't finished typechecking, this type can be non-normal by the
  -- time it's used again, so make sure to normalize there!
  ty' <- termAnn te'
  ty <- freshTypeUnifyVar' (render $ "cast" <+> pPrint te') (Just (UnifyConstraintCasted ty'))
  return $ TermPrimitive (PrimitiveCast te') ty
synthPrimitive (PrimitiveTuple tes) = do
  tes' <- synthTerm `traverse` tes
  return $ TermPrimitive (PrimitiveTuple tes') (TypeTuple <$> mapM termAnn tes')
synthPrimitive (PrimitiveArray tes) = do
  ty <- freshTypeUnifyVar' (render $ "primitive array" <+> pPrint tes) Nothing
  tes' <- synthCheckTerm' ty `traverse` tes
  return $ TermPrimitive (PrimitiveArray tes') (TypeArray <$> ty)
synthPrimitive (PrimitiveIf te te1 te2) = do
  te' <- synthCheckTerm TypeBit te
  ty <- freshTypeUnifyVar' (render $ "primitive if branches" <+> pPrint [te1, te2]) Nothing
  te1' <- synthCheckTerm' ty te1
  te2' <- synthCheckTerm' ty te2
  return $ TermPrimitive (PrimitiveIf te' te1' te2') ty
synthPrimitive (PrimitiveAnd te1 te2) = do
  te1' <- synthCheckTerm TypeBit te1
  te2' <- synthCheckTerm TypeBit te2
  return $ TermPrimitive (PrimitiveAnd te1' te2') (return TypeBit)
synthPrimitive (PrimitiveOr te1 te2) = do
  te1' <- synthCheckTerm TypeBit te1
  te2' <- synthCheckTerm TypeBit te2
  return $ TermPrimitive (PrimitiveOr te1' te2') (return TypeBit)
synthPrimitive (PrimitiveNot te) = do
  te' <- synthCheckTerm TypeBit te
  return $ TermPrimitive (PrimitiveNot te') (return TypeBit)
synthPrimitive (PrimitiveEq te1 te2) = do
  ty <- freshTypeUnifyVar' (render $ "primitive equality" <+> pPrint [te1, te2]) Nothing
  te1' <- synthCheckTerm' ty te1
  te2' <- synthCheckTerm' ty te2
  return $ TermPrimitive (PrimitiveEq te1' te2') ty
synthPrimitive (PrimitiveAdd te1 te2) = do
  ty <- freshTypeUnifyVar' (render $ "primitive add" <+> pPrint [te1, te2]) (Just UnifyConstraintNumeric)
  te1' <- synthCheckTerm' ty te1
  te2' <- synthCheckTerm' ty te2
  return $ TermPrimitive (PrimitiveAdd te1' te2') ty
synthPrimitive (PrimitiveExtends te tyId) = error "synthPrimitive PrimitiveExtends"

synthCheckTerm :: Type -> Term () -> TypingM (Term MType)
synthCheckTerm type_ term = do
  term' <- synthTerm term
  checkTerm type_ term'
  return term'

synthCheckTerm' :: MType -> Term () -> TypingM (Term MType)
synthCheckTerm' m_type term = do
  type_ <- m_type
  synthCheckTerm type_ term

-- ** Checking

checkTerm :: Type -> Term MType -> TypingM ()
checkTerm expectType term = do
  synthType <- termAnn term
  unify expectType synthType

checkTerm' :: MType -> Term MType -> TypingM ()
checkTerm' mExpectType term = do
  expectType <- mExpectType
  checkTerm expectType term

synthCheckPattern' :: MType -> Pattern () -> TypingM (Pattern MType)
synthCheckPattern' mtype (PatternNamed ti ()) = return $ PatternNamed ti mtype
synthCheckPattern' mtype (PatternDiscard ()) = return $ PatternDiscard mtype
synthCheckPattern' _mtype (PatternConstructor _mb_tyId _tmId _pats ()) = error "TODO: synthCheckPattern PatternConstructor"

introPattern :: Pattern MType -> TypingM a -> TypingM a
introPattern pat@(PatternNamed tmId mty) m = do
  let app =
        Applicant
          { applicantMaybeTypeId = Nothing,
            applicantTermId = tmId,
            applicantAnn = ApplicantType mty
          }
  localM (execStateT $ modifyInsertUnique (ctxApplicants . at (void app)) app (TypingError ("attempted to shadow" <+> ticks (pPrint (void app))) (Just $ toSyntax (void pat)))) m
-- introTermId tmId mty m
introPattern (PatternDiscard _) m = m
introPattern (PatternConstructor _tyId _ctorId pats _) m = comps (pats <&> introPattern) m

-- ** Unification

-- | Attempt to unify an expected type with a synthesized type.
-- > <expected type> ~? <synthesized type>
unify :: Type -> Type -> TypingM ()
unify type1 type2 = case (type1, type2) of
  -- TypeUnifyVar
  (TypeUnifyVar uv mb_uc, ty) -> substUnifyVar type1 type2 uv mb_uc ty -- substitute uv for ty while unifying type1 and type2
  (ty, TypeUnifyVar uv mb_uc) -> substUnifyVar type1 type2 uv mb_uc ty
  -- simple types
  (TypeNumber numty1 size1, TypeNumber numty2 size2) -> unless (numty1 == numty2 && size1 == size2) $ throwUnifyError type1 type2 Nothing
  (TypeBit, TypeBit) -> return ()
  (TypeChar, TypeChar) -> return ()
  -- complex types
  (TypeArray ty1, TypeArray ty2) -> unify ty1 ty2
  (TypeTuple tys1, TypeTuple tys2) -> uncurry unify `mapM_` (tys1 `zip` tys2)
  (TypeOptional ty1, TypeOptional ty2) -> unify ty1 ty2
  -- named types are only equal if they have the same name
  (TypeNamed tyId1, TypeNamed tyId2) | tyId1 == tyId2 -> return ()
  -- non-unifiable types
  (tyExpect, tySynth) -> throwUnifyError tyExpect tySynth Nothing

unify' :: MType -> MType -> TypingM ()
unify' mtype1 mtype2 = do
  type1 <- mtype1
  type2 <- mtype2
  unify type1 type2

-- uv{mb_uc} := ty (during: type1 ~ type2)
substUnifyVar :: Type -> Type -> UnifyVar -> Maybe UnifyConstraint -> Type -> TypingM ()
substUnifyVar type1 type2 uv mb_uc ty = do
  -- check if uv1 occurs in type2
  when (uv `unifyVarOccursInType` ty) $ throwUnifyError type1 type2 (Just "fails occurs check")
  case mb_uc of
    Nothing -> return ()
    -- check if ty satisfies the constraints uc
    Just uc -> unless (ty `satisfiesUnifyConstraint` uc) $ throwUnifyError type1 type2 (Just $ "it does not satisfy unification constraint:" <+> pPrint uc)
  modifyingM (envUnification . at uv) \case
    Just ty' -> FlexM.throw $ "trying to substitute" <+> ticks (pPrint uv) <+> "for" <+> ticks (pPrint ty) <+> ", but it's already be substituted for" <+> pPrint ty'
    Nothing -> return $ Just ty

satisfiesUnifyConstraint :: Type -> UnifyConstraint -> Bool
satisfiesUnifyConstraint ty = \case
  UnifyConstraintCasted ty' -> case (ty, ty') of
    (TypeNumber numty1 _size1, TypeNumber numty2 _size2)
      | all (`elem` [TypeInt, TypeUInt]) [numty1, numty2] -> True
      | all (`elem` [TypeFloat]) [numty1, numty2] -> True
    (TypeUnifyVar _ mb_uc, _) -> maybe True (satisfiesUnifyConstraint ty') mb_uc
    -- !TODO FlexM.throw $ FlexLog "typing" $ "this case of `satisfiesUnifyConstraint` is not implemented yet:" $$ nest 4 ("type =" <+> pPrint ty) $$ nest 4 ("unifyConstraint =" <+> pPrint uc)
    _uc -> False
  UnifyConstraintNumeric -> case ty of
    TypeNumber _ _ -> True
    _ -> False

unifyVarOccursInType :: UnifyVar -> Type -> Bool
unifyVarOccursInType uv = \case
  TypeArray ty -> unifyVarOccursInType uv ty
  TypeTuple tys -> any (unifyVarOccursInType uv) tys
  TypeOptional ty -> unifyVarOccursInType uv ty
  TypeUnifyVar uv' _ | uv == uv' -> True
  _ -> False

throwUnifyError :: Type -> Type -> Maybe Doc -> TypingM a
throwUnifyError tyExpect tySynth mb_msg =
  throwTypingError
    ( "failed to unify synthesized type"
        $$ nest 2 (pPrint tySynth)
        $$ "with expected type"
        $$ nest 2 (pPrint tyExpect)
        $$ maybe mempty ("because" <+>) mb_msg
    )
    Nothing

freshTypeUnifyVar :: String -> Maybe UnifyConstraint -> TypingM Type
freshTypeUnifyVar str mb_uc = do
  i <- gets (^. envFreshUnificationVarIndex)
  modifying envFreshUnificationVarIndex (+ 1)
  return $ TypeUnifyVar (UnifyVar str i) mb_uc

freshTypeUnifyVar' :: String -> Maybe UnifyConstraint -> TypingM MType
freshTypeUnifyVar' str mb_uc' = normalizeType <$> freshTypeUnifyVar str mb_uc'