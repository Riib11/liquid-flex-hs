module Language.Flex.Typing where

import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import qualified Data.Map as Map
import Data.Maybe
import Language.Flex.FlexM (MonadFlex)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Syntax as Syntax
import Language.Flex.Typing.Module
import Language.Flex.Typing.TypingM
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum)

-- ** Typing

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
synthDeclaration decl = FlexM.markSection
  [FlexM.FlexMarkStep ("synthDeclaration:" <+> pPrintDeclarationHeader decl) Nothing]
  case decl of
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
  let mb_syn = Just $ SyntaxDeclaration $ DeclarationFunction fun
  let FunctionType {..} = functionType
  let intros :: TypingCtx -> TypingM TypingCtx
      intros = execStateT do
        -- intro params
        forM_ functionParameters \(paramId, paramType) -> do
          -- assert param type is normal
          paramType' <- lift . return $ assertNormalType paramType
          let app =
                Applicant
                  { applicantTermId = paramId,
                    applicantOutputAnn = paramType'
                  }
          let protoapp = fromApplicantToProtoApplicant app
          -- add applicant
          modifyInsertUnique mb_syn (void app) (ctxApplicants . at protoapp) app

        -- intro contextual params
        forM_ functionContextualParameters $ mapM \(cxparamNewtypeId, cxparamId) -> do
          cxparamType' <- lift . return $ assertNormalType $ TypeNamed cxparamNewtypeId
          let app =
                Applicant
                  { applicantTermId = cxparamId,
                    applicantOutputAnn = cxparamType'
                  }
          let protoapp = fromApplicantToProtoApplicant app
          -- add applicant
          modifyInsertUnique mb_syn (void app) (ctxApplicants . at protoapp) app
          -- add to contextual parameters
          modifyInsertUnique mb_syn cxparamId (ctxCxparamNewtypeIds . at cxparamId) cxparamNewtypeId
          modifyInsertUnique mb_syn cxparamId (ctxCxparamIds . at cxparamNewtypeId) cxparamId
  localM intros do
    functionBody1 <- synthCheckTerm' (normalizeType functionOutput) functionBody
    functionBody2 <- normalizeInternalTypes functionBody1
    functionBody3 <- defaultInternalTypes functionBody2
    return fun {functionBody = functionBody3}

synthConstant :: Constant Type () -> TypingM (Constant Type Type)
synthConstant con@Constant {..} = do
  constantBody1 <- synthCheckTerm' (normalizeType constantType) constantBody
  constantBody2 <- normalizeInternalTypes constantBody1
  constantBody3 <- defaultInternalTypes constantBody2
  return con {constantBody = constantBody3}

synthRefinedType :: RefinedType () -> TypingM (RefinedType Type)
synthRefinedType rt@(RefinedType {..}) = do
  -- get the fields to add to scope
  fields <-
    asks (^. ctxTypes . at refinedTypeId) >>= \case
      Nothing -> FlexM.throw "should have found that this is an unknown refined type id during construction of TypingCtx"
      Just ctxType -> case ctxType of
        (CtxStructure Structure {..}) -> return structureFields
        (CtxNewtype Newtype {..}) -> return [(newtypeFieldId, newtypeType)]
        _ -> FlexM.throw "should have found that this is not a refinable type's id during construction of TypingCtx"
  comps
    ( fields <&> \(fieldId, fieldMType) -> do
        let mb_syn = Just $ SyntaxDeclaration $ DeclarationRefinedType rt
        let tmId = fromFieldIdToTermId fieldId
        let app =
              Applicant
                { applicantTermId = tmId,
                  applicantOutputAnn = fieldMType
                }
        let protoapp = fromApplicantToProtoApplicant app
        localM . execStateT $
          modifyInsertUnique mb_syn tmId (ctxApplicants . at protoapp) app
    )
    do
      refinedTypeRefinement1 <- synthRefinement refinedTypeRefinement
      refinedTypeRefinement2 <- normalizeInternalTypes refinedTypeRefinement1
      refinedTypeRefinement3 <- defaultInternalTypes refinedTypeRefinement2
      return rt {refinedTypeRefinement = refinedTypeRefinement3}

synthRefinement :: Refinement () -> TypingM (Refinement MType)
synthRefinement (Refinement tm) = Refinement <$> synthCheckTerm TypeBit tm

synthTerm :: Term () -> TypingM (Term MType)
synthTerm term0 = FlexM.markSection
  [FlexM.FlexMarkStep ("synthTerm:" <+> pPrint term0) Nothing]
  case term0 of
    (TermLiteral lit ()) -> TermLiteral lit <$> synthLiteral lit
    (TermPrimitive prim ()) -> synthPrimitive prim
    (TermLet mb_tmId te1 te2 ()) -> do
      te1' <- synthTerm te1
      te2' <- case mb_tmId of
        Nothing -> introPattern (PatternDiscard (termAnn te1')) $ synthTerm te2
        Just tmId -> introPattern (PatternNamed tmId (termAnn te1')) $ synthTerm te2
      return $ TermLet mb_tmId te1' te2' (termAnn te2')
    (TermAssert tm1 tm2 ()) -> do
      tm1' <- synthCheckTerm TypeBit tm1
      tm2' <- synthTerm tm2
      return $ TermAssert tm1' tm2' (termAnn tm2')
    (TermStructure structId fields ()) ->
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
    (TermMember tm fieldId ()) -> do
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
    (TermNeutral _ ()) -> FlexM.throw $ "TermNeutral should only appear as an OUTPUT of type synthesis, not as an INPUT:" <+> pPrint term0
    (TermProtoNeutral ProtoNeutral {..} ()) -> do
      let mb_syn = Just $ toSyntax term0
      lookupApplicant protoNeutralProtoApplicant >>= \app -> case app of
        (ApplicantFunction {..}) -> do
          FunctionType {..} <- lookupFunctionType applicantFunctionId

          -- assert that correct number of args are given
          args <- case protoNeutralMaybeArgs of
            Nothing -> throwTypingError "function application requires a list of arguments" mb_syn
            Just args -> return args
          unless (length args == length functionParameters) $ throwTypingError ("incorrect number of arguments given to function application; expected" <+> pPrint (length functionParameters) <+> "but got" <+> pPrint (length args)) mb_syn

          -- synth-check args
          args' <- forM (functionParameters `zip` args) \((_argId, argMType), argTerm) -> synthCheckTerm' argMType argTerm

          mb_cxargs' <- case functionContextualParameters of
            Nothing -> do
              -- assert that no contextual args are given
              unless (isNothing protoNeutralMaybeCxargs) $ throwTypingError "function application provided explicit contextual arguments when the function does not have contextual parameters" mb_syn
              return Nothing
            Just cxparams ->
              do
                case protoNeutralMaybeCxargs of
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
                            throwTypingError ("could not infer the implicit contextual arguments:" <+> commaList (missing <&> \(cxparamId, cxparamNewtypeId) -> parens (pPrint cxparamId <+> ":" <+> pPrint cxparamNewtypeId))) mb_syn
                          else do
                            -- all cxargs were inferred
                            return . flip concatMap ls_mb_cxarg $
                              maybe [] . comp1 pure $ \(cxargId, cxNewtypeId) ->
                                let cxargMType = normalizeType $ TypeNamed cxNewtypeId
                                 in TermNeutral
                                      { termNeutral = Neutral {neutralTermId = cxargId},
                                        termAnn = cxargMType
                                      }
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
          return $
            TermNeutral
              { termNeutral =
                  NeutralFunctionApplication
                    { neutralFunctionId = functionId,
                      neutralArgs = args',
                      neutralMaybeCxargs = mb_cxargs'
                    },
                termAnn = applicantOutputAnn
              }
        (ApplicantEnumConstructor {..}) ->
          lookupType applicantEnumId
            >>= \case
              (CtxEnum Enum {..}) -> do
                -- assert no args
                unless (isNothing protoNeutralMaybeCxargs) $ throwTypingError "an enum construction cannot have arguments" mb_syn
                -- assert no cxargs
                unless (isNothing protoNeutralMaybeCxargs) $ throwTypingError "an enum construction cannot have contextual arguments" mb_syn
                return $
                  TermNeutral
                    { termNeutral =
                        NeutralEnumConstruction
                          { neutralEnumId = enumId,
                            neutralConstructorId = applicantConstructorId
                          },
                      termAnn = applicantOutputAnn
                    }
              ctxType -> FlexM.throw $ "the context stores that the applicant is an enum constructor," <+> ticks (pPrint app) <+> "but the type corresponding to the type id is not an enum," <+> ticks (pPrint ctxType)
        (ApplicantVariantConstructor {..}) -> do
          lookupType applicantVariantId >>= \case
            CtxVariant Variant {..} -> do
              -- assert no cxargs
              unless (isNothing protoNeutralMaybeCxargs) $ throwTypingError "a variant construction cannot have contextual arguments" mb_syn
              -- synth-check args
              args <- case protoNeutralMaybeArgs of
                Nothing -> throwTypingError "a variant construction must have arguments" mb_syn
                Just args -> return args
              params <- case applicantConstructorId `lookup` variantConstructors of
                Nothing -> throwTypingError "the variant doesn't have the constructor" mb_syn
                Just params -> return params
              args' <- forM (params `zip` args) (uncurry synthCheckTerm')
              return $
                TermNeutral
                  { termNeutral =
                      NeutralVariantConstruction
                        { neutralVariantId = variantId,
                          neutralConstructorId = applicantConstructorId,
                          neutralArgs = args'
                        },
                    termAnn = applicantOutputAnn
                  }
            ctxType -> FlexM.throw $ "the context stores that the applicant is a variant constructor," <+> ticks (pPrint app) <+> "but the type corresponding to the type id is not a variant," <+> ticks (pPrint ctxType)
        (ApplicantNewtypeConstructor {..}) -> do
          lookupType applicantNewtypeId >>= \case
            CtxNewtype Newtype {..} -> do
              -- assert no cxarsg
              unless (isNothing protoNeutralMaybeCxargs) $ throwTypingError "a newtype construction cannot have contextual arguments" mb_syn
              -- assert single arg
              arg <- case protoNeutralMaybeArgs of
                Just [arg] -> return arg
                _ -> throwTypingError "a newtype construction must have exactly one argument" mb_syn
              -- synth-check arg
              arg' <- synthCheckTerm' newtypeType arg
              return $
                TermNeutral
                  { termNeutral =
                      NeutralNewtypeConstruction
                        { neutralNewtypeId = newtypeId,
                          neutralConstructorId = applicantConstructorId,
                          neutralArg = arg'
                        },
                    termAnn = applicantOutputAnn
                  }
            ctxType -> FlexM.throw $ "the context stores that the applicant is a newtype constructor," <+> ticks (pPrint app) <+> "but the type corresponding to the type id is not a newtype," <+> ticks (pPrint ctxType)
        (Applicant {..}) -> do
          -- assert no args
          unless (isNothing protoNeutralMaybeArgs) $ throwTypingError "arguments given to a non-function" mb_syn
          -- assert no cxargs
          unless (isNothing protoNeutralMaybeCxargs) $ throwTypingError "explicit contextual arguments given to a non-function" mb_syn
          return
            TermNeutral
              { termNeutral = Neutral {neutralTermId = applicantTermId},
                termAnn = applicantOutputAnn
              }
    (TermAscribe tm ty ()) ->
      -- unwraps TermAscribe, so TermAscribe should not appear in a typed term
      synthCheckTerm' (normalizeType ty) tm
    (TermMatch tm branches ()) -> do
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
  -- PrimitiveCase is unwrapped during typing, so should not appear in typed
  -- result
  te' <- synthTerm te
  -- since we haven't finished typechecking, this type can be non-normal by the
  -- time it's used again, so make sure to normalize there!
  ty' <- termAnn te'
  ty <- freshTypeUnifyVar' (render $ "cast" <+> pPrint te') (Just (UnifyConstraintCasted ty'))
  return te' {termAnn = ty}
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
  return $ TermPrimitive (PrimitiveEq te1' te2') (return TypeBit)
synthPrimitive (PrimitiveAdd te1 te2) = do
  ty <- freshTypeUnifyVar' (render $ "primitive add" <+> pPrint [te1, te2]) (Just UnifyConstraintNumeric)
  te1' <- synthCheckTerm' ty te1
  te2' <- synthCheckTerm' ty te2
  return $ TermPrimitive (PrimitiveAdd te1' te2') ty
synthPrimitive (PrimitiveExtends {}) = error "synthPrimitive PrimitiveExtends"

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
  runExceptT (unify expectType synthType) >>= \case
    Left (mb_doc, tyExpect, tySynth) ->
      throwTypingError
        ( vcat
            [ hcat ["When checking that", pPrint term, "has type", pPrint expectType, ", failed to unify synthesized type"],
              nest 2 (pPrint tySynth),
              "with expected type",
              nest 2 (pPrint tyExpect),
              maybe mempty ("because" <+>) mb_doc
            ]
        )
        (Just $ SyntaxTerm (void term))
    Right () -> return ()

checkTerm' :: MType -> Term MType -> TypingM ()
checkTerm' mExpectType term = do
  expectType <- mExpectType
  checkTerm expectType term

synthCheckPattern' :: MType -> Pattern () -> TypingM (Pattern MType)
synthCheckPattern' mtype (PatternNamed ti ()) = return $ PatternNamed ti mtype
synthCheckPattern' mtype (PatternDiscard ()) = return $ PatternDiscard mtype
synthCheckPattern' _mtype (PatternConstructor _mb_tyId _tmId _pats ()) = error "TODO: synthCheckPattern PatternConstructor"

introPattern :: Pattern MType -> TypingM a -> TypingM a
introPattern pat@(PatternNamed tmId mty) = do
  let app =
        Applicant
          { applicantTermId = tmId,
            applicantOutputAnn = mty
          }
  let protoapp = fromApplicantToProtoApplicant app
  localM . execStateT $
    modifyInsertUnique (Just $ SyntaxPattern (void pat)) app (ctxApplicants . at protoapp) app
introPattern (PatternDiscard _) = id
introPattern (PatternConstructor _tyId _ctorId pats _) = comps (pats <&> introPattern)

-- ** Unification

type UnifyM a = ExceptT (Maybe Doc, Type, Type) TypingM a

-- | Attempt to unify an expected type with a synthesized type.
-- > <expected type> ~? <synthesized type>
unify :: Type -> Type -> UnifyM ()
unify type1 type2 = case (type1, type2) of
  -- type unification variables; substitute uv for ty while unifying type1 and type2
  (TypeUnifyVar uv mb_uc, ty) -> substUnifyVar type1 type2 uv mb_uc ty
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

unify' :: MType -> MType -> UnifyM ()
unify' mtype1 mtype2 = do
  type1 <- lift mtype1
  type2 <- lift mtype2
  unify type1 type2

-- uv{mb_uc} := ty (during: type1 ~ type2)
substUnifyVar :: Type -> Type -> UnifyVar -> Maybe UnifyConstraint -> Type -> UnifyM ()
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
    (TypeBit, TypeBit) -> True
    (TypeChar, TypeChar) -> True
    (TypeArray ty1, TypeArray ty2) -> satisfiesUnifyConstraint ty1 (UnifyConstraintCasted ty2)
    -- (TypeTuple tys1, TypeTuple tys2) -> all (\(ty1, ty2) -> satisfiesUnifyConstraint ty1 (UnifyConstraintCasted ty2)) $ tys1 `zip` tys2
    -- (TypeOptional ty1, TypeOptional ty2) -> satisfiesUnifyConstraint ty1 (UnifyConstraintCasted ty2)
    (TypeUnifyVar _ mb_uc, _) -> maybe True (satisfiesUnifyConstraint ty') mb_uc
    -- !TODO FlexM.throw $ FlexLog "typing" $ "this case of `satisfiesUnifyConstraint` is not implemented yet:" $$ nest 2 ("type =" <+> pPrint ty) $$ nest 2 ("unifyConstraint =" <+> pPrint uc)
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

throwUnifyError :: Type -> Type -> Maybe Doc -> UnifyM a
throwUnifyError tyExpect tySynth mb_msg =
  throwError
    ( mb_msg,
      tyExpect,
      tySynth
    )

-- ( "failed to unify synthesized type"
--     $$ nest 2 (pPrint tySynth)
--     $$ "with expected type"
--     $$ nest 2 (pPrint tyExpect)
--     $$ maybe mempty ("because" <+>) mb_msg
-- )
-- Nothing

freshTypeUnifyVar :: String -> Maybe UnifyConstraint -> TypingM Type
freshTypeUnifyVar str mb_uc = do
  i <- gets (^. envFreshUnificationVarIndex)
  modifying envFreshUnificationVarIndex (+ 1)
  return $ TypeUnifyVar (UnifyVar str i) mb_uc

freshTypeUnifyVar' :: String -> Maybe UnifyConstraint -> TypingM MType
freshTypeUnifyVar' str mb_uc' = normalizeType <$> freshTypeUnifyVar str mb_uc'