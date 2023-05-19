{-# HLINT ignore "Redundant return" #-}
module Language.Flex.Typing where

import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import Data.Foldable (foldlM)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Traversable as Traversable
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
typeModule mdl0 = FlexM.markSection [FlexM.FlexMarkStep "typeModule" . Just $ pPrint mdl0] do
  ctx <- moduleTypingCtx mdl0
  env <- moduleTypingEnv mdl0
  (env', mdl4) <- runTypingM' env ctx do
    mdl1 <- synthModule mdl0
    -- aliases can also appear in types that are necessarily given during
    -- parsing, so need to normalize those also
    mdl2 <- unTy <$> normalizeAndDefaultInternalTypes (Ty mdl1)
    mdl3 <- unTm <$> normalizeAndDefaultInternalTypes (Tm mdl2)
    -- make sure to synthModule before getting state
    env' <- get
    return (env', mdl3)
  return (env', mdl4)

-- ** Synthesizing

-- | Type annotations are defaulted and normalized.
synthModule :: Module Type () -> TypingM (Module Type Type)
synthModule mdl@Module {..} = do
  moduleDeclarations' <- forM moduleDeclarations synthDeclaration
  return mdl {moduleDeclarations = moduleDeclarations'}

synthDeclaration :: Declaration Type () -> TypingM (Declaration Type Type)
synthDeclaration decl = FlexM.markSection [FlexM.FlexMarkStep ("synthDeclaration:" <+> pPrintDeclarationHeader decl) Nothing] do
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
          paramType' <- lift $ assertNormalType paramType
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
          cxparamType' <- lift $ assertNormalType $ TypeNamed cxparamNewtypeId
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
    functionBody1 <- synthCheckTerm functionOutput functionBody
    return fun {functionBody = functionBody1}

synthConstant :: Constant Type () -> TypingM (Constant Type Type)
synthConstant con@Constant {..} = do
  constantBody1 <- synthCheckTerm constantType constantBody
  return con {constantBody = constantBody1}

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
    ( fields <&> \(fieldId, fieldType) -> do
        let mb_syn = Just $ SyntaxDeclaration $ DeclarationRefinedType rt
        let tmId = fromFieldIdToTermId fieldId
        let app =
              Applicant
                { applicantTermId = tmId,
                  applicantOutputAnn = fieldType
                }
        let protoapp = fromApplicantToProtoApplicant app
        localM . execStateT $
          modifyInsertUnique mb_syn tmId (ctxApplicants . at protoapp) app
    )
    do
      refinedTypeRefinement1 <- synthRefinement refinedTypeRefinement
      return rt {refinedTypeRefinement = refinedTypeRefinement1}

synthRefinement :: Refinement () -> TypingM (Refinement Type)
synthRefinement (Refinement tm) = Refinement <$> synthCheckTerm TypeBit tm

synthTerm :: Term () -> TypingM (Term Type)
synthTerm term0 = FlexM.markSection
  [FlexM.FlexMarkStep ("synthTerm:" <+> pPrint term0) Nothing]
  case term0 of
    (TermLiteral lit ()) -> TermLiteral lit <$> synthLiteral lit
    (TermPrimitive prim ()) -> synthPrimitive prim
    (TermLet mb_tmId te1 te2 ()) -> do
      te1' <- synthTerm te1
      te2' <- case mb_tmId of
        Nothing -> synthTerm te2
        Just tmId -> introTermId tmId (termAnn te1') $ synthTerm te2
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
          fields' <- forM fieldItems \(fieldId, (fieldType, fieldTerm)) -> do
            fieldTerm' <- synthCheckTerm fieldType fieldTerm
            return (fieldId, fieldTerm')
          let structType = TypeNamed structId
          return $ TermStructure structId fields' structType
        ctxType -> throwTypingError ("the type id" <+> ticks (pPrint structId) <+> "was expected to be of a struct, but it is actually of" <+> ticks (pPrintDeclarationHeader (fromCtxTypeToDeclaration ctxType))) (Just $ toSyntax term0)
    (TermMember tm fieldId ()) -> do
      tm' <- synthTerm tm
      normalizeType (termAnn tm') >>= \case
        (TypeNamed tyId) ->
          lookupType tyId >>= \case
            (CtxStructure Structure {..}) -> do
              fieldType <- case fieldId `lookup` structureFields of
                Nothing -> throwTypingError ("the structure" <+> ticks (pPrint structureId) <+> "does not have the field" <+> ticks (pPrint fieldId)) (Just $ toSyntax term0)
                Just fieldType -> return fieldType
              return $ TermMember tm' fieldId fieldType
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
          args' <- forM (functionParameters `zip` args) \((_argId, argType), argTerm) -> synthCheckTerm argType argTerm

          mb_cxargs' :: Maybe [Term Type] <- case functionContextualParameters of
            Nothing -> do
              -- assert that no contextual args are given
              unless (isNothing protoNeutralMaybeCxargs) $ throwTypingError "function application provided explicit contextual arguments when the function does not have contextual parameters" mb_syn
              return Nothing
            Just cxparams -> do
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
                  cxargs :: [Term Type] <-
                    runWriterT cxargsWriter >>= \(ls_mb_cxarg, missing) ->
                      if not (null missing)
                        then do
                          -- there are some cxargs missing
                          throwTypingError ("could not infer the implicit contextual arguments:" <+> commaList (missing <&> \(cxparamId, cxparamNewtypeId) -> parens (pPrint cxparamId <+> ":" <+> pPrint cxparamNewtypeId))) mb_syn
                        else do
                          -- all cxargs were inferred
                          fmap concat . Traversable.for ls_mb_cxarg . maybe (return []) $ \(cxargId, cxNewtypeId) -> do
                            cxargType <- normalizeType $ TypeNamed cxNewtypeId
                            return
                              [ TermNeutral
                                  { termNeutral = Neutral {neutralTermId = cxargId},
                                    termAnn = cxargType
                                  }
                              ]
                  return $ Just cxargs
                -- check explicit contextual args
                Just cxargs -> do
                  -- synth and check that each cxarg has a newtype type
                  cxargs' <- forM cxargs \cxarg -> do
                    cxarg' <- synthTerm cxarg
                    normalizeType (termAnn cxarg') >>= \case
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
              args' <- forM (params `zip` args) (uncurry synthCheckTerm)
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
              arg' <- synthCheckTerm newtypeType arg
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
      synthCheckTerm ty tm
    (TermMatch tm branches ()) -> do
      tm' <- synthTerm tm
      ty <- freshTypeUnifyVar' (render $ "match result" <+> pPrint term0) Nothing
      branches' <- forM branches (synthCheckBranch (termAnn tm') ty)
      return $ TermMatch tm' branches' ty

synthCheckBranch :: Type -> Type -> Branch () -> TypingM (Branch Type)
synthCheckBranch inType outType (pat, tm) = do
  pat' <- synthCheckPattern' inType pat
  tm' <- introPattern pat' $ synthCheckTerm outType tm
  return (pat', tm')

synthLiteral :: Literal -> TypingM Type
synthLiteral (LiteralInteger n) = freshTypeUnifyVar' (render $ "literal integer" <+> pPrint n) (pure $ UnifyConstraintCasted (TypeNumber TypeInt 32))
synthLiteral (LiteralFloat x) = freshTypeUnifyVar' (render $ "literal float" <+> pPrint x) (pure $ UnifyConstraintCasted (TypeNumber TypeFloat 32))
synthLiteral (LiteralBit b) = freshTypeUnifyVar' (render $ "literal bit" <+> pPrint b) (pure $ UnifyConstraintCasted TypeBit)
synthLiteral (LiteralChar c) = freshTypeUnifyVar' (render $ "literal char" <+> pPrint c) (pure $ UnifyConstraintCasted TypeChar)
synthLiteral (LiteralString s) = freshTypeUnifyVar' (render $ "literal string" <+> pPrint s) (pure $ UnifyConstraintCasted (TypeArray TypeChar))

synthPrimitive :: Primitive () -> TypingM (Term Type)
synthPrimitive (PrimitiveTry te) = do
  te' <- synthTerm te
  return $ TermPrimitive (PrimitiveTry te') (TypeOptional $ termAnn te')
synthPrimitive PrimitiveNone = do
  TermPrimitive PrimitiveNone <$> freshTypeUnifyVar' "None" Nothing
synthPrimitive (PrimitiveSome tm) = do
  tm' <- synthTerm tm
  return $ TermPrimitive (PrimitiveSome tm') (TypeOptional $ termAnn tm')
synthPrimitive (PrimitiveCast te) = do
  te' <- synthTerm te
  -- since we haven't finished typechecking, this type can be non-normal by the
  -- time it's used again, so make sure to normalize there!
  ty' <- normalizeType (termAnn te')
  ty <- freshTypeUnifyVar' (render $ "cast" <+> pPrint te') (Just (UnifyConstraintCasted ty'))
  return $ TermPrimitive (PrimitiveCast te') ty
synthPrimitive (PrimitiveTuple tes) = do
  tes' <- synthTerm `traverse` tes
  return $ TermPrimitive (PrimitiveTuple tes') (TypeTuple $ termAnn <$> tes')
synthPrimitive (PrimitiveArray tes) = do
  ty <- freshTypeUnifyVar' (render $ "primitive array" <+> pPrint tes) Nothing
  tes' <- synthCheckTerm ty `traverse` tes
  return $ TermPrimitive (PrimitiveArray tes') (TypeArray ty)
synthPrimitive (PrimitiveIf te te1 te2) = do
  te' <- synthCheckTerm TypeBit te
  ty <- freshTypeUnifyVar' (render $ "primitive if branches" <+> pPrint [te1, te2]) Nothing
  te1' <- synthCheckTerm ty te1
  te2' <- synthCheckTerm ty te2
  return $ TermPrimitive (PrimitiveIf te' te1' te2') ty
synthPrimitive (PrimitiveBoolBinOp bbo te1 te2) = do
  te1' <- synthCheckTerm TypeBit te1
  te2' <- synthCheckTerm TypeBit te2
  return $ TermPrimitive (PrimitiveBoolBinOp bbo te1' te2') TypeBit
synthPrimitive (PrimitiveNot te) = do
  te' <- synthCheckTerm TypeBit te
  return $ TermPrimitive (PrimitiveNot te') TypeBit
synthPrimitive (PrimitiveEq neg te1 te2) = do
  ty <- freshTypeUnifyVar' (render $ "primitive equality" <+> pPrint [te1, te2]) Nothing
  te1' <- synthCheckTerm ty te1
  -- !TODO why do i need to normalize again here; shouldn't that be handled already via being wrapped in a TypingM?
  te2' <- synthCheckTerm ty te2
  return $ TermPrimitive (PrimitiveEq neg te1' te2') TypeBit
synthPrimitive (PrimitiveNumBinOp nbo te1 te2) = do
  ty <- freshTypeUnifyVar' (render $ "primitive" <+> text (operatorOfNumBinOp nbo) <+> pPrint [te1, te2]) (Just UnifyConstraintNumeric)
  te1' <- synthCheckTerm ty te1
  te2' <- synthCheckTerm ty te2
  return $ TermPrimitive (PrimitiveNumBinOp nbo te1' te2') ty
synthPrimitive (PrimitiveNumBinRel nbr te1 te2) = do
  ty <- freshTypeUnifyVar' (render $ "primitive" <+> text (operatorOfNumBinRel nbr) <+> pPrint [te1, te2]) (Just UnifyConstraintNumeric)
  te1' <- synthCheckTerm ty te1
  te2' <- synthCheckTerm ty te2
  return $ TermPrimitive (PrimitiveNumBinRel nbr te1' te2') TypeBit
synthPrimitive (PrimitiveExtends {}) = error "synthPrimitive PrimitiveExtends"
synthPrimitive PrimitiveException = TermPrimitive PrimitiveException <$> freshTypeUnifyVar' "exception" Nothing

synthCheckTerm :: Type -> Term () -> TypingM (Term Type)
synthCheckTerm type_ term = FlexM.markSection [FlexM.FlexMarkStep ("synthCheckTerm (" <> pPrint type_ <> ") (" <> pPrint term <> ")") Nothing] do
  type' <- normalizeType type_
  term' <- synthTerm term
  checkTerm type' term'
  return term'

-- ** Checking

-- Tries to unify the types. If successful, then updated environment as if
-- normal unify. If unsuccessful, does not update environment.
tryUnify :: Type -> Type -> TypingM (Either (Maybe Doc, Type, Type) ())
tryUnify tyExpect tyInfer = do
  env <- get
  lift (runStateT (runExceptT (unify tyExpect tyInfer)) env) >>= \case
    (Left err, _env') -> return (Left err)
    (Right _, env') -> do
      put env'
      return (Right ())

checkTerm :: Type -> Term Type -> TypingM ()
checkTerm expectType term = do
  synthType <- normalizeType $ termAnn term
  runExceptT (unify expectType synthType) >>= \case
    Left (mb_doc, tyExpect, tySynth) ->
      throwTypingError
        ( vcat
            [ hsep ["When checking that", pPrint term, "has type", pPrint expectType, ", failed to unify synthesized type"],
              nest 2 (pPrint tySynth),
              "with expected type",
              nest 2 (pPrint tyExpect),
              maybe mempty ("because" <+>) mb_doc
            ]
        )
        (Just $ SyntaxTerm (void term))
    Right () -> return ()

synthCheckPattern' :: Type -> Pattern () -> TypingM (Pattern Type)
synthCheckPattern' type_ pat@(PatternConstructor tyId' ctorId tmIds ()) =
  case type_ of
    TypeNamed tyId | tyId == tyId' -> return $ PatternConstructor tyId ctorId tmIds type_
    ty -> throwTypingError ("can't match on term of type" <+> ticks (pPrint ty)) (Just $ SyntaxPattern pat)
synthCheckPattern' type_ pat@(PatternNone ()) =
  case type_ of
    (TypeOptional _ty) -> return $ PatternNone type_
    ty -> throwTypingError ("can't match on term of type" <+> ticks (pPrint ty)) (Just $ SyntaxPattern pat)
synthCheckPattern' type_ pat@(PatternSome tmId ()) =
  case type_ of
    (TypeOptional _ty) -> return $ PatternSome tmId type_
    ty -> throwTypingError ("can't match on term of type" <+> ticks (pPrint ty)) (Just $ SyntaxPattern pat)

introTermId :: TermId -> Type -> TypingM a -> TypingM a
introTermId tmId ty m = do
  ty' <- normalizeType ty
  let app =
        Applicant
          { applicantTermId = tmId,
            applicantOutputAnn = ty'
          }
  let protoapp = fromApplicantToProtoApplicant app
  ( localM . execStateT $
      modifyInsertUnique Nothing app (ctxApplicants . at protoapp) app
    )
    m

introPattern :: Pattern Type -> TypingM a -> TypingM a
introPattern (PatternConstructor tyId ctorId tmIds _) m = do
  lookupType tyId >>= \case
    CtxVariant Variant {..} -> do
      tys <- case ctorId `lookup` variantConstructors of
        Nothing -> FlexM.throw $ "variant" <+> ticks (pPrint variantId) <+> "does not have constructor" <+> ticks (pPrint ctorId)
        Just tys -> return tys
      let tmIds_tys = tmIds `zip` tys
      comps (tmIds_tys <&> uncurry introTermId) m
    CtxEnum _enum -> do
      -- enum constructor doesn't have parameters, so can't introduce anything
      m
    _ -> FlexM.throw $ "attempted to match on a term of type:" <+> ticks (pPrint tyId)
introPattern (PatternNone _) m = m
introPattern (PatternSome tmId ty) m =
  case ty of
    TypeOptional ty' -> introTermId tmId ty' m
    _ -> FlexM.throw $ "PatternSome should not have been typed with type" <+> ticks (pPrint ty)

-- ** Unification

type UnifyM a = ExceptT (Maybe Doc, Type, Type) TypingM a

-- | Attempt to unify an expected type with a synthesized type.
-- > <expected type> ~? <synthesized type>
unify :: Type -> Type -> UnifyM ()
unify type1 type2 = FlexM.markSection [FlexM.FlexMarkStep ("unify (" <> pPrint type1 <> ") (" <> pPrint type2 <> ")") Nothing] case (type1, type2) of
  -- type unification variables; substitute uv for ty while unifying type1 and type2
  (TypeUnifyVar uv, ty) -> substUnifyVar type1 type2 uv ty
  (ty, TypeUnifyVar uv) -> substUnifyVar type1 type2 uv ty
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

unify' :: Type -> Type -> UnifyM ()
unify' type1 type2 = do
  type1' <- lift $ normalizeType type1
  type2' <- lift $ normalizeType type2
  unify type1' type2'

substUnifyVar :: Type -> Type -> UnifyVar -> Type -> UnifyM ()
substUnifyVar type1 type2 uv ty = FlexM.markSection [FlexM.FlexMarkStep ("substUnifyVar; uv = " <> pPrint uv <> "; ty = " <> pPrint ty) Nothing] do
  case ty of
    -- already identical
    TypeUnifyVar uv' | uv == uv' -> return ()
    _ -> do
      gets (^. envUnification . at uv) >>= \case
        Just (Right ty') -> FlexM.throw $ "trying to substitute" <+> ticks (pPrint uv) <+> "for" <+> ticks (pPrint ty) <+> ", but it's already be substituted for" <+> pPrint ty'
        -- uv is constrained by uc
        Just (Left uc) ->
          lift (ty `satisfiesUnifyConstraint` uc) >>= \case
            False -> throwUnifyError type1 type2 (Just $ "it does not satisfy unification constraint:" <+> pPrint uc)
            True -> envUnification . at uv ?= Right ty
        -- uv is unconstrained
        Nothing -> envUnification . at uv ?= Right ty

satisfiesUnifyConstraint :: Type -> UnifyConstraint -> TypingM Bool
satisfiesUnifyConstraint ty1 uc2 = case uc2 of
  UnifyConstraintCasted ty2 -> case (ty1, ty2) of
    (TypeNumber numty1 _size1, TypeNumber numty2 _size2)
      | all (`elem` [TypeInt, TypeUInt]) [numty1, numty2] -> return True
      | all (`elem` [TypeFloat]) [numty1, numty2] -> return True
    (TypeBit, TypeBit) -> return True
    (TypeChar, TypeChar) -> return True
    (TypeArray ty1', TypeArray ty2') -> satisfiesUnifyConstraint ty1' (UnifyConstraintCasted ty2')
    (TypeTuple tys1, TypeTuple tys2) -> allM (\(ty1', ty2') -> satisfiesUnifyConstraint ty1' (UnifyConstraintCasted ty2')) $ tys1 `zip` tys2
    (TypeOptional ty1', TypeOptional ty2') -> satisfiesUnifyConstraint ty1' (UnifyConstraintCasted ty2')
    -- ty = uv
    (TypeUnifyVar uv1, _) ->
      -- !TODO FlexM.throw $ FlexLog "typing" $ "this case of `satisfiesUnifyConstraint` is not implemented yet:" $$ nest 2 ("type =" <+> pPrint ty) $$ nest 2 ("unifyConstraint =" <+> pPrint uc)
      gets (^. envUnification . at uv1) >>= \case
        Nothing -> maybe (return True) (satisfiesUnifyConstraint ty2) Nothing
        -- ty1 is constrained by uc1
        Just (Left uc1) ->
          runExceptT (leqUnifyConstraint uc1 uc2) >>= \case
            (Left _err) -> return False
            (Right b) -> return b
        -- ty is a unify var substituted for ty''
        Just (Right ty_) -> FlexM.throw $ "in satisfiesUnifyConstraint; expected inputs to be normalized, but ty is a unify var that is substiuted for" <+> pPrint ty_
    _uc -> return False
  UnifyConstraintNumeric -> case ty1 of
    TypeNumber _ _ -> return True
    TypeUnifyVar uv1 ->
      gets (^. envUnification . at uv1) >>= \case
        -- already substituted
        Just (Right ty1') -> normalizeType ty1' >>= (`satisfiesUnifyConstraint` uc2)
        -- neither constrained nor substituted
        Nothing -> return True
        -- constrained but not yet substituted
        Just (Left uc) -> case uc of
          (UnifyConstraintCasted ty') -> satisfiesUnifyConstraint ty' UnifyConstraintNumeric
          UnifyConstraintNumeric -> return True
    _ -> return False

leqUnifyConstraint :: UnifyConstraint -> UnifyConstraint -> UnifyM Bool
leqUnifyConstraint (UnifyConstraintCasted ty1) (UnifyConstraintCasted ty2) =
  lift (tryUnify ty1 ty2) >>= \case
    (Left _err) -> return False
    (Right _) -> return True
leqUnifyConstraint uc (UnifyConstraintCasted ty) = lift $ satisfiesUnifyConstraint ty uc
leqUnifyConstraint (UnifyConstraintCasted ty) uc = lift $ satisfiesUnifyConstraint ty uc
leqUnifyConstraint UnifyConstraintNumeric UnifyConstraintNumeric = return True

allM :: ((Type, Type) -> TypingM Bool) -> [(Type, Type)] -> TypingM Bool
allM k =
  foldlM
    ( \case
        False -> \_ -> return False
        True -> k
    )
    True

unifyVarOccursInType :: UnifyVar -> Type -> Bool
unifyVarOccursInType uv = \case
  TypeArray ty -> unifyVarOccursInType uv ty
  TypeTuple tys -> any (unifyVarOccursInType uv) tys
  TypeOptional ty -> unifyVarOccursInType uv ty
  TypeUnifyVar uv' | uv == uv' -> True
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
  envFreshUnificationVarIndex %= (+ 1)
  let uv = UnifyVar str i
  case mb_uc of
    Just uc -> envUnification . at uv ?= Left uc
    Nothing -> return ()
  return $ TypeUnifyVar uv

freshTypeUnifyVar' :: String -> Maybe UnifyConstraint -> TypingM Type
freshTypeUnifyVar' str mb_uc' = normalizeType =<< freshTypeUnifyVar str mb_uc'