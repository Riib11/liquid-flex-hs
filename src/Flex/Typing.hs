{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Flex.Typing where

import Control.Category ((>>>))
import Control.Lens hiding (enum)
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Bifunctor
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (Maybe (..), isJust)
import Data.Text (Text)
import Flex.Flex
import Flex.Syntax
import qualified Flex.Unif as Unif
import PrettyShow
import Utility

{- NOTES

# Type Normalization

Since types are usually pretty small, normalization is cheap, so its not a huge
deal that I often have to normalize types, even if they are already normalized.
But to be more efficient, could reduce repeated normaliations if I know a type
is already normal. To do this I need to statically keep track of whether or not
a type is normalized.

-}

-- ** typing monad

-- type Typing = FlexM TypingM

type Typing = ReaderT Ctx FlexM

runTyping :: Ctx -> Typing a -> FlexM a
runTyping ctx m = runReaderT m ctx

tryTyping :: Typing a -> Typing (Maybe a)
tryTyping m = do
  env <- get
  ctx <- ask
  (lift . lift . lift) (runFlexM env (runTyping ctx m)) >>= \case
    Left _err -> return Nothing
    Right (a, env) -> do
      put env
      return $ Just a

data Ctx = Ctx
  { -- | local variable typings
    _ctxLocals :: Map.Map Text Type,
    -- | which things are currently being type-checked
    _typingStack :: [TypingStackItem]
  }
  deriving (Show)

data TypingStackItem
  = TypingStackCheck Syntax
  | TypingStackInfer Syntax
  | TypingStackUnify
      Type
      -- ^ inferred type
      Type
      -- ^ expected type
  deriving (Show)

instance PrettyShow TypingStackItem where
  prettyShow = \case
    TypingStackCheck syn -> "check " <> prettyShow syn
    TypingStackInfer syn -> "infer " <> prettyShow syn
    TypingStackUnify ty ty' -> prettyShow ty <> "  ~?  " <> prettyShow ty'

emptyCtx :: Ctx
emptyCtx =
  Ctx
    { _ctxLocals = mempty,
      _typingStack = mempty
    }

-- *** lenses

makeLenses ''Ctx

-- *** utilities

enterCheck :: IsSyntax a => a -> Typing b -> Typing b
enterCheck a = locally typingStack (TypingStackCheck (toSyntax a) :)

enterInfer :: IsSyntax a => a -> Typing b -> Typing b
enterInfer a = locally typingStack (TypingStackInfer (toSyntax a) :)

enterUnify :: Type -> Type -> Typing b -> Typing b
enterUnify ty1 ty2 = locally typingStack (TypingStackUnify ty1 ty2 :)

-- *** errors/bugs

-- type error
throwTypingError :: String -> Typing a
throwTypingError = throwError . TypingError

-- bug in typing algorithm
throwTypingBug :: String -> Typing a
throwTypingBug msg = do
  tyStack <- asks (^. typingStack)
  lift $
    throwBug
      ( "typing",
        msg
          <> "\n\ntyping stack:\n"
          <> List.intercalate "\n" ((" > " <>) . prettyShow <$> tyStack)
      )

-- ** processing

-- expects environment to already be loaded
procModule :: Module -> Typing ()
procModule mdl = enterCheck mdl do
  debug $ "procModule: " <> prettyShow (moduleId mdl)
  mapM_ procImport (moduleImports mdl)
  mapM_ procDeclaration (moduleDeclarations mdl)

-- add imported stuff to module envModuleCtx
procImport :: Import -> Typing ()
procImport _imp =
  -- TODO: forally outline how module system impl works
  return ()

-- check is a valid type id
procTypeId :: Id -> Typing ()
procTypeId x =
  unlessM (isJust <$> gets (^. envModuleCtx . ctxModuleTypes . at x)) $ throwTypingError $ "unknown type id: " <> prettyShow x

-- check is an existing structure id
procStructureId :: Id -> Typing ()
procStructureId x =
  gets (^. envModuleCtx . ctxModuleTypes . at x) >>= \case
    Just (DeclarationTypeStructure _) -> return ()
    Nothing -> throwTypingError $ "unknown structure id: " <> prettyShow x
    _ -> throwTypingError $ "expected to be a structure id: " <> prettyShow x

-- -- check is an existing term id
-- procTermId :: Id -> Typing ()
-- procTermId x =
--   asks (^. ctxLocals . at x) >>= \case
--     Just _ -> return ()
--     Nothing ->
--       gets (^. envModuleCtx . ctxModuleConstants . at) >>= \case
--         Just _ -> return ()
--         Nothing -> throwTypingError $ "unknown term id: " <> prettyShow x

-- -- check is an existing constructor id
-- procConstructorId :: Id -> Typing ()
-- procConstructorId = error "TODO"

-- checks terms and norms types, and updates ModuleCtx to reflect updates
procDeclaration :: Declaration -> Typing ()
procDeclaration decl = enterCheck decl do
  debug $ "procDeclaration: " <> prettyShow (get_name decl)
  case decl of
    DeclarationStructure struct -> do
      procTypeId `mapM_` structureExtensionId struct
      procStructureId `mapM_` structureExtensionId struct
      structureFields <- normType `mapM` structureFields struct
      structureRefinement <- procRefinement (structureRefinement struct)
      envModuleCtx
        . ctxModuleTypes
        . at (fromUnqualName $ structureName struct)
        .= Just (DeclarationTypeStructure struct {structureFields, structureRefinement})
    DeclarationNewtype newty -> do
      newtypeType <- normType $ newtypeType newty
      newtypeRefinement <- procRefinement $ newtypeRefinement newty
      envModuleCtx
        . ctxModuleTypes
        . at (fromUnqualName $ newtypeName newty)
        .= Just (DeclarationTypeNewtype newty {newtypeType, newtypeRefinement})
    DeclarationVariant varnt -> do
      variantConstructors <- normType `mapM` variantConstructors varnt
      envModuleCtx
        . ctxModuleTypes
        . at (fromUnqualName $ variantName varnt)
        .= Just (DeclarationTypeVariant varnt {variantConstructors})
    DeclarationEnumerated enm -> do
      unless (isLiteralType (enumeratedLiteralType enm)) $ throwTypingError $ "the type of an enum must be a literal type, but instead it is: " <> prettyShow (enumeratedLiteralType enm)
      envModuleCtx
        . ctxModuleTypes
        . at (fromUnqualName $ enumeratedName enm)
        .= Just (DeclarationTypeEnumerated enm)
    DeclarationAlias alias -> do
      aliasType <- normType $ aliasType alias
      envModuleCtx
        . ctxModuleTypes
        . at (fromUnqualName $ aliasName alias)
        .= Just (DeclarationTypeAlias alias {aliasType})
    DeclarationFunction fun -> do
      let funTy = functionType fun
      functionType <-
        if functionIsTransform fun
          then normTransformFunctionType $ functionType fun
          else normFunctionType $ functionType fun
      functionBody <-
        ffoldr312
          (functionTypeParams funTy <> (fmap (first Just) . Map.toList $ functionTypeContextualParams funTy))
          (\(mb_txt, ty) -> maybe id (`introLocal` ty) mb_txt)
          ( procDefinitionBody (functionTypeOutput funTy) $
              functionBody fun
          )
      envModuleCtx
        . ctxModuleFunctions
        . at (fromUnqualName $ functionName fun)
        .= Just (fun {functionType, functionBody})
    DeclarationConstant con -> do
      constantType <- normType $ constantType con
      constantBody <- procDefinitionBody constantType $ constantBody con
      let con' = con {constantType, constantBody}
      debug $ "procDeclaration: " <> prettyShow (get_name decl) <> " ==> " <> prettyShow con'
      envModuleCtx
        . ctxModuleConstants
        . at (fromUnqualName $ constantName con)
        .= Just con'

normFunctionType :: FunctionType -> Typing FunctionType
normFunctionType funTy = do
  debug $ "normFunctionType: " <> prettyShow funTy
  functionTypeParams <- secondM normType `mapM` functionTypeParams funTy
  functionTypeContextualParams <- normType `mapM` functionTypeContextualParams funTy
  functionTypeOutput <- normType $ functionTypeOutput funTy
  return funTy {functionTypeParams, functionTypeContextualParams, functionTypeOutput}

normTransformFunctionType :: FunctionType -> Typing FunctionType
normTransformFunctionType funTy = do
  debug $ "normTransformFunctionType: " <> prettyShow funTy
  -- check that the input and output types are messages
  functionTypeParams <- forM (functionTypeParams funTy) \(txt, ty) -> do
    ty <- normType ty
    unlessM (isMessageType ty) $ throwTypingError $ "the parameter '" <> prettyShow txt <> ": " <> prettyShow ty <> "' must be a message type in order to be the parameter of a transform function"
    return (txt, ty)
  functionTypeContextualParams <- normType `mapM` functionTypeContextualParams funTy
  functionTypeOutput <- normType (functionTypeOutput funTy)
  unlessM (isMessageType functionTypeOutput) $ throwTypingError $ "the function output type '" <> prettyShow functionTypeOutput <> "' must be a message type in order to be the output of a transform function"
  unlessM (isMessageType functionTypeOutput) $ throwTypingError $ "the output type '" <> prettyShow functionTypeOutput <> "' must be a message type in order to be the output of a transform function"
  return funTy {functionTypeParams, functionTypeContextualParams, functionTypeOutput}

-- expects type to be normalized
isMessageType :: Type -> Typing Bool
isMessageType ty = do
  unless (isNorm ty) $ throwTypingBug $ "isMessageType expects input type to be normalized: " <> prettyShow ty
  case ty of
    TypeStructure struct -> return $ get_isMessage struct
    TypeNewtype newty -> return $ get_isMessage newty
    _ -> return False

procDefinitionBody :: Type -> DefinitionBody -> Typing DefinitionBody
procDefinitionBody ty = \case
  DefinitionBodyTerm tm -> DefinitionBodyTerm <$> liftM2' checkTerm (inferTerm tm) (return ty)
  DefinitionBodyDerived (Just tm) -> DefinitionBodyDerived . Just <$> liftM2' checkTerm (inferTerm tm) (return ty)
  body -> return body

procIntroId :: Id -> Typing ()
procIntroId x@(Id mb_mdlId _) = case mb_mdlId of
  Nothing -> return ()
  Just _mdlId -> throwTypingError $ "unnecessary qualification of introduced id: " <> prettyShow x

procRefinement :: Refinement -> Typing Refinement
procRefinement rfn@(Refinement Nothing) = return rfn
procRefinement (Refinement (Just tm)) = do
  tm <- inferTerm tm
  Refinement . Just <$> checkTerm tm TypeBit

-- -- normalize a type, which expands named types and applies unification
-- -- substitution
-- normType :: Type -> Typing Type
-- normType = \case
--   TypeArray ty -> normType ty
--   TypeOptional ty -> normType ty
--   TypeNamed x -> lookupType x
--   TypeCast ty -> normType ty
--   TypeUnif uf ->
--   ty -> return ty

isNorm :: Type -> Bool
isNorm = \case
  TypeArray ty -> isNorm ty
  TypeOptional ty -> isNorm ty
  TypeCast ty -> isNorm ty
  TypeNamed _ -> False
  _ -> True

checkInferTerm :: Term -> Type -> Typing Term
checkInferTerm tm ty = do
  tm <- inferTerm tm
  checkTerm tm ty

-- annotate the term with it's inferred type, and proc its children
inferTerm :: Term -> Typing Term
inferTerm tm = enterInfer tm do
  debug $ "inferTerm: " <> prettyShow tm
  case tm ^. termPreterm of
    -- without kids
    TermLiteral (LiteralInteger _) -> return $ setType (TypeCast (TypeInt (IntSize 32)))
    TermLiteral (LiteralFloat _) -> return $ setType (TypeCast (TypeFloat FloatSize32))
    TermLiteral (LiteralBit _) -> return $ setType (TypeCast TypeBit)
    TermLiteral (LiteralChar _) -> return $ setType (TypeCast TypeChar)
    TermLiteral (LiteralString _) -> return $ setType (TypeCast (TypeArray TypeChar))
    TermNamed x -> do
      locs <- asks (^. ctxLocals)
      (setType <$>) $
        (normType =<<) $
          lookupTerm throwTypingError locs x getInferredType
    TermCast tm -> do
      tm <- inferTerm tm
      ty <- getInferredType tm
      return $ setPretermAndType (tm ^. termPreterm) (TypeCast ty)
    TermArray tms -> do
      ty <- freshUnifType (prettyShow tm)
      (ty, tms) <-
        foldrM
          ( \tm (ty, tms) -> do
              tm <- liftM2' checkTerm (inferTerm tm) (return ty)
              ty <- getInferredType tm
              return (ty, tm : tms)
          )
          (ty, [])
          tms
      return $ setPretermAndType (TermArray tms) (TypeArray ty)
    TermTuple tms -> do
      tms <- inferTerm `mapM` tms
      tys <- getInferredType `traverse` tms
      return $ setPretermAndType (TermTuple tms) (TypeTuple tys)
    TermBlock (stmts, tm) -> do
      (stmts, tm) <- checkBlock (stmts, tm)
      ty <- getInferredType tm
      return $ setPretermAndType (TermBlock (stmts, tm)) ty
    -- decide whether this is a variant constructor or newtype constructor
    TermConstructor x mb_tm ->
      lookupConstructor throwTypingError x >>= \case
        ConstructorEnumerated enm _lit ->
          case mb_tm of
            Just _ -> throwTypingError "an enum constructor must not have any arguments"
            Nothing -> return $ setPretermAndType (TermConstructor x Nothing) (TypeEnumerated enm)
        ConstructorNewtype newty ->
          case mb_tm of
            Nothing -> throwTypingError "a newtype constructor must have an argument"
            Just tm -> do
              tm <- inferTerm tm
              tm <- checkTerm tm (newtypeType newty)
              return $ setPretermAndType (TermConstructor x (Just tm)) (TypeNewtype newty)
        ConstructorVariant varnt (_, ty) ->
          case mb_tm of
            Nothing -> throwTypingError "a variant constructor must have an argument"
            Just tm -> do
              tm <- inferTerm tm
              tm <- checkTerm tm ty
              return $ setPretermAndType (TermConstructor x (Just tm)) (TypeVariant varnt)
    TermApplication (AppPrimFun pf) args cxargs -> inferAppPrimFun pf args cxargs
    TermApplication (AppId x) args cxargs -> do
      fun <- lookupFunction throwTypingError x

      funTy <- return $ functionType fun
      debug $ "funTy = " <> prettyShow funTy

      -- check arguments length
      let n_args_expected = length (functionTypeParams funTy)
      let n_args_actual = length args
      when (n_args_actual /= n_args_expected) $ throwTypingError $ "the function `" <> prettyShow x <> "` was given too " <> (if n_args_actual < n_args_expected then "few" else "many") <> " arguments; expected " <> show n_args_expected <> " but actually got " <> show n_args_actual

      -- check arguments
      args <-
        (\(arg, (_mb_txt, ty)) -> checkInferTerm arg =<< normType ty)
          `mapM` (args `zip` functionTypeParams funTy)

      -- check contextual arguments, and infer any implicit (i.e. not explicitly
      -- given) contextual arguments
      cxargsMap <- case cxargs of
        Nothing -> do
          locs <- asks (^. ctxLocals)
          Map.fromList
            <$> ( ( \(txt, ty) ->
                      findM
                        (\(_txtLoc, tyLoc) -> isJust <$> tryTyping (checkUnify tyLoc ty))
                        (Map.toList locs)
                        >>= \case
                          Nothing -> throwTypingError $ "could not infer value of contextual parameter: " <> prettyShow txt
                          Just (txt, ty) -> return (ty, makeTerm (TermNamed (fromUnqualName txt)) ty)
                  )
                    `traverse` Map.toList (functionTypeContextualParams funTy)
                )
        Just (Left cxargsList) ->
          Map.fromList
            <$> ( \tm' -> do
                    tm' <- inferTerm tm'
                    mb_txt_ty <-
                      findMapM
                        ( \(_txt, ty) ->
                            tryTyping (checkTerm tm' ty) >>= \case
                              -- this argument doesn't have the type of this
                              -- contextual parameter, so can't be the argument for
                              -- this contextual parameter
                              Nothing -> return Nothing
                              -- this argument does have the type of this contextual
                              -- parameter, and since the types of contextual
                              -- parameters must be unique, this is the argument for
                              -- this contextual parameter
                              Just tm' -> return (Just (ty, tm'))
                        )
                        (Map.toList . functionTypeContextualParams $ funTy)
                    case mb_txt_ty of
                      Nothing -> throwTypingError $ "missing explicit contextual argument: " <> prettyShow tm
                      Just (ty, tm') -> return (ty, tm')
                )
              `traverse` cxargsList
        Just (Right _) -> throwTypingBug "contextual arguments should not already be type-checked here"
      debug $ "inferTerm: output preterm = " <> prettyShow (TermApplication (AppId x) args (Just . Right $ cxargsMap))
      return $ setPretermAndType (TermApplication (AppId x) args (Just . Right $ cxargsMap)) (functionTypeOutput funTy)
    TermStructure x fields -> do
      struct <- lookupStructure throwTypingError x
      fields <-
        mapAsListM
          ( \(txt, field) -> do
              case structureFields struct Map.!? txt of
                Nothing -> throwTypingError $ "the structure '" <> prettyShow x <> "' does not have the field '" <> prettyShow txt <> "'"
                Just ty -> (txt,) <$> checkInferTerm field ty
          )
          fields
      setPretermAndType (TermStructure x fields) <$> normType (TypeNamed x)
    TermMember tm txt -> do
      tm <- inferTerm tm
      getInferredType tm >>= \case
        TypeStructure struct ->
          case Map.lookup txt (structureFields struct) of
            Nothing -> throwTypingError $ "the structure '" <> prettyShow struct <> "' does not have field '" <> prettyShow txt <> "'"
            Just ty -> setPretermAndType (TermMember tm txt) <$> normType ty
        _ -> throwTypingError $ "expected '" <> prettyShow tm <> "' to be of a structure type"
    TermIf tm1 tm2 tm3 -> do
      tm1 <- checkInferTerm tm1 TypeBit
      ty <- freshUnifType (prettyShow tm)
      tm2 <- checkInferTerm tm2 ty
      tm3 <- checkInferTerm tm3 ty
      setPretermAndType (TermIf tm1 tm2 tm3) <$> normType ty
    TermAscribe tm ty -> do
      -- unwraps the ascription via typechecking
      tm <- inferTerm tm
      ty <- normType ty
      checkTerm tm ty
    TermMatch _ _ -> error "TODO: inferTerm TermMatch"
  where
    {- TermEq tm1 tm2 -> do
      ty <- freshUnifType (prettyShow tm)
      tm1 <- checkInferTerm tm1 ty
      tm2 <- checkInferTerm tm2 ty
      setPretermAndType (TermEq tm1 tm2) <$> normType ty -}

    setPretermAndType :: Preterm -> Type -> Term
    setPretermAndType pt ty =
      tm
        & termMaybeType %~ \case
          Nothing -> Just ty
          Just _ -> error "tried to initialize the type of a term that already had an initialized type"
        & termPreterm .~ pt

    setType :: Type -> Term
    setType = setPretermAndType (tm ^. termPreterm)

checkBlock :: ([Statement], Term) -> Typing ([Statement], Term)
checkBlock (stmts, tm0) = procStatements [] stmts
  where
    procStatements :: [Statement] -> [Statement] -> Typing ([Statement], Term)
    procStatements stmts' [] = (reverse stmts',) <$> inferTerm tm0
    procStatements stmts' (stmt : stmts) = case stmt of
      StatementLet pat tm -> do
        -- TODO: warn about shadowing
        ty <- freshUnifType (prettyShow stmt)
        withProcessCheckPattern ty pat \pat -> do
          tm <- liftM2' checkTerm (inferTerm tm) (return ty)
          procStatements (StatementLet pat tm : stmts') stmts
      StatementAssert tm -> do
        tm <- liftM2' checkTerm (inferTerm tm) (return TypeBit)
        procStatements (StatementAssert tm : stmts) stmts'

withProcessCheckPattern :: Type -> Pattern -> (Pattern -> Typing a) -> Typing a
withProcessCheckPattern ty pat k = case pat ^. patternPrepattern of
  PatternDiscard ->
    k (pat & patternType ?~ ty)
  PatternNamed x ->
    locally ctxLocals (Map.insert x ty) $ k (pat & patternType ?~ ty)
  PatternLiteral lit -> do
    ty <- liftM2' checkUnify (inferLiteral lit) (return ty)
    k (pat & patternType ?~ ty)

introLocal :: Text -> Type -> Typing a -> Typing a
introLocal x ty = locally ctxLocals (Map.insert x ty)

-- ** type checking

-- check that term has expected type via unification with its inferred type, and
-- normalize the term's type (with the new unification substitution)
checkTerm :: Term -> Type -> Typing Term
checkTerm tm ty = enterCheck tm do
  debug $ "checkTerm: " <> prettyShow tm <> " :? " <> prettyShow ty
  ty <- join $ liftM2 checkUnify (getInferredType tm) (return ty)
  return $ tm & termMaybeType ?~ ty

-- ** type inference

getInferredType :: Term -> Typing Type
getInferredType tm = case tm ^. termMaybeType of
  Nothing -> throwTypingBug $ "tried to get inferred type of a term before before inference: " <> prettyShow tm
  Just ty -> normType ty

inferLiteral :: Literal -> Typing Type
inferLiteral = \case
  LiteralInteger _ -> return (TypeCast (TypeInt (IntSize 32)))
  LiteralFloat _ -> return (TypeCast (TypeFloat FloatSize32))
  LiteralBit _ -> return (TypeCast TypeBit)
  LiteralChar _ -> return (TypeCast TypeChar)
  LiteralString _ -> return (TypeCast (TypeArray TypeChar))

-- *** unification

-- | checks if `tyInf` unities with `tyExp` (directional), and returns `tyExp`
-- after normalizing.
checkUnify :: Type -> Type -> Typing Type
checkUnify tyInf tyExp = enterUnify tyInf tyExp do
  debug $ "checkUnify: " <> prettyShow tyInf <> " ~? " <> prettyShow tyExp
  case (tyInf, tyExp) of
    -- a-normal types
    _
      | not (isNorm tyInf) -> throwTypingBug $ "anormal type during checkUnify: " <> prettyShow tyInf
      | not (isNorm tyExp) -> throwTypingBug $ "anormal type during checkUnify: " <> prettyShow tyExp
    -- unification variable
    (TypeUnif u, ty) -> substUnifId u ty
    (ty, TypeUnif u) -> substUnifId u ty
    -- base types
    -- TODO: check that int/uint sizes are valid (i.e. are non-zero)
    (TypeInt s1, TypeInt s2) | s1 == s2 -> return tyExp
    (TypeUInt s1, TypeUInt s2) | s1 == s2 -> return tyExp
    (TypeFloat s1, TypeFloat s2) | s1 == s2 -> return tyExp
    -- polymorphic types
    (TypeArray ty1, TypeArray ty2) -> TypeArray <$> checkUnify ty1 ty2
    (TypeTuple tys1, TypeTuple tys2) | length tys1 == length tys2 -> TypeTuple <$> zipWithM checkUnify tys1 tys2
    (TypeOptional ty1, TypeOptional ty2) -> TypeOptional <$> checkUnify ty1 ty2
    -- casting
    -- TODO: if ty2 is a Cast also, then should be able to cast
    (TypeCast ty1, _) -> tyExp <$ unless (isCastableTo ty1 tyExp) (throwTypingError $ "cannot cast the type '" <> prettyShow ty1 <> "' to the type '" <> prettyShow tyExp <> "'")
    -- equality checks (includes checking named types)
    _ | tyInf == tyExp -> return tyExp
    -- unification failure
    _ | otherwise -> tyExp <$ failure
  where
    failure = throwTypingError $ "expected the type '" <> prettyShow tyExp <> "' but found the type '" <> prettyShow tyInf <> "'"

-- requires normal `ty`
substUnifId :: Unif.Id -> Type -> Typing Type
substUnifId u ty = do
  if occursIn ty
    then throwTypingError $ "tried to substitute '" <> prettyShow u <> "' for '" <> prettyShow ty <> "', during unification, but failed non-occurence check"
    else envUnifSubst . at u .= Just ty -- TODO: this does proper insertion right?
  return (TypeUnif u)
  where
    occursIn :: Type -> Bool
    occursIn = \case
      TypeArray ty -> occursIn ty
      TypeTuple tys -> any occursIn tys
      TypeOptional ty -> occursIn ty
      TypeCast ty -> occursIn ty
      TypeUnif u' -> u == u'
      _ -> False

isExtensionOf :: Id -> Id -> Typing Bool
isExtensionOf x xExt
  | x == xExt = return True
  | otherwise =
      gets (^. envModuleCtx . ctxModuleTypes . at x) >>= \case
        Nothing -> False <$ throwTypingError ("unknown id: " <> prettyShow x)
        Just (DeclarationTypeStructure struct) -> case structureExtensionId struct of
          Nothing -> return False
          Just x' -> x' `isExtensionOf` xExt
        _ -> False <$ throwTypingError ("id must be a structure id: " <> prettyShow x)

isCastableTo :: Type -> Type -> Bool
isCastableTo ty1 ty2 = case (ty1, ty2) of
  (_, TypeCast ty2') -> isCastableTo ty1 ty2'
  _ | castGroup ty1 == castGroup ty2 -> True
  _ -> ty1 == ty2
  where
    castGroup :: Type -> Maybe String
    castGroup = \case
      TypeUInt _ -> Just "integral"
      TypeInt _ -> Just "integral"
      TypeFloat _ -> Just "floating"
      _ -> Nothing

-- ** normalization

-- expands named types and applies current unifying substitution
normType :: Type -> Typing Type
normType = \case
  TypeArray ty -> TypeArray <$> normType ty
  TypeTuple tys -> TypeTuple <$> mapM normType tys
  TypeOptional ty -> TypeOptional <$> normType ty
  TypeCast ty -> TypeCast . unwrapCasts <$> normType ty
  TypeNamed x -> lookupType throwTypingError x
  TypeUnif u ->
    gets (^. envUnifSubst . at u) >>= \case
      Nothing -> return (TypeUnif u)
      Just ty -> return ty
  ty -> return ty

unwrapCasts :: Type -> Type
unwrapCasts = \case
  TypeCast ty -> unwrapCasts ty
  ty -> ty

-- ** primitives

-- the type of a primitive function can depend on the type of its arguments, so
-- needs a special inference algorithm
inferAppPrimFun :: PrimFun -> [Term] -> Maybe (Either [Term] (Map.Map Type Term)) -> Typing Term
inferAppPrimFun pf args cxargs = case (pf, args, cxargs) of
  (PrimFunEq, [tm1, tm2], Nothing) -> do
    -- infer type of tm1
    tm1 <- inferTerm tm1
    -- check that tm2 has tm1's type
    tm2 <- checkInferTerm tm2 =<< getInferredType tm1
    return_ [tm1, tm2] cxargs TypeBit
  (PrimFunAnd, [tm1, tm2], Nothing) -> booleanBinop tm1 tm2
  (PrimFunOr, [tm1, tm2], Nothing) -> booleanBinop tm1 tm2
  (PrimFunNot, [tm], Nothing) -> do
    tm <- checkInferTerm tm TypeBit
    return_ [tm] cxargs TypeBit
  (PrimFunPlus, [tm1, tm2], Nothing) -> numericBinop tm1 tm2
  (PrimFunDiv, [tm1, tm2], Nothing) -> numericBinop tm1 tm2
  _ -> throwTypingError $ "invalid primitive function application: " <> prettyShow (TermApplication (AppPrimFun pf) args cxargs)
  where
    booleanBinop tm1 tm2 = do
      tm1 <- checkInferTerm tm1 TypeBit
      tm2 <- checkInferTerm tm2 TypeBit
      return_ [tm1, tm2] cxargs TypeBit

    numericBinop tm1 tm2 = do
      -- TODO: handle this for more than just int32, need to do something
      -- slightly complicated with casting, because the result type of `a + b`
      -- needs to be the same numeric type as `a` and `b` each have, which could
      -- be casts, which means that the result of the whole `a + b` should have
      -- the casted numeric type also, but when that cast is
      -- instantiated/defaulted, should affect the casts on the types of the
      -- internal terms as well

      -- -- infer type of tm1
      -- tm1 <- inferTerm tm1
      -- ty1 <- getInferredType tm1
      -- -- check that ty1 is a numeric type
      -- unless (isNumericType ty1) . throwTypingError $ "the primitive function application " <> prettyShow (TermApplication (AppPrimFun pf) args cxargs) <> " expects its argument " <> prettyShow tm1 <> " to have a numeric type, instead of " <> prettyShow ty1
      -- -- check that tm2 has type ty1
      -- tm2 <- checkInferTerm tm2 ty1
      -- return_ [tm1, tm2] cxargs ty1
      tm1 <- checkInferTerm tm1 (TypeInt (IntSize 32))
      tm2 <- checkInferTerm tm2 (TypeInt (IntSize 32))
      return_ [tm1, tm2] cxargs (TypeInt (IntSize 32))

    return_ args cxargs ty =
      return
        Term
          { _termPreterm = TermApplication (AppPrimFun pf) args cxargs,
            _termMaybeType = Just ty
          }

-- ** defaulting

-- | Some types can be defaulted if they are not constrained by type-checking.
-- For example, cast types (which only exist during type-checking) that inferred
-- but never checked against an expected type can be defaulted to a certain
-- instance of the casted type. A particular example: defaultType
-- `'cast(int<n>)' = 'int<n>'`.
-- - eliminates `cast` forms
-- - expects type to be normal
defaultType :: Type -> Type
defaultType = \case
  TypeArray ty -> TypeArray $ defaultType ty
  TypeTuple tys -> TypeTuple $ defaultType <$> tys
  TypeOptional ty -> TypeOptional $ defaultType ty
  -- defaults to casted type
  TypeCast ty -> ty
  ty -> ty

defaultTerm :: Term -> Term
defaultTerm tm = tm & termMaybeType %~ fmap defaultType

-- ** utilities

freshUnifId :: String -> Typing Unif.Id
freshUnifId str = do
  eu <- (^. envUnif) <$> get
  let (u, eu') = Unif.freshId str eu
  modify (envUnif .~ eu')
  return u

freshUnifType :: String -> Typing Type
freshUnifType str = TypeUnif <$> freshUnifId str

freshenFunctionTypeUnifIds :: FunctionType -> Typing FunctionType
freshenFunctionTypeUnifIds funTy = do
  (m, paramsRev) <-
    foldM
      ( \(m, params) (mb_txt, a) ->
          go m a >>= \(m', a') -> return (m', (mb_txt, a') : params)
      )
      (Map.empty, [] :: [(Maybe Text, Type)])
      (functionTypeParams funTy)
  let functionTypeParams = reverse paramsRev
  -- TODO: freshen contextual params
  -- (m, cxparams) <-
  --   foldM
  --     ( \(m, cxparams) (txt, )
  --     )
  (_m, functionTypeOutput) <- go m (functionTypeOutput funTy)
  return
    FunctionType
      { functionTypeParams,
        functionTypeContextualParams = functionTypeContextualParams funTy,
        functionTypeOutput
      }
  where
    go :: Map.Map Unif.Id Unif.Id -> Type -> Typing (Map.Map Unif.Id Unif.Id, Type)
    go m = \case
      TypeUnif u -> case Map.lookup u m of
        Just u' -> return (m, TypeUnif u')
        Nothing -> do
          u' <- freshUnifId (Unif.getLabel u)
          return (Map.insert u u' m, TypeUnif u')
      TypeArray a -> go m a >>= \(m, a') -> return (m, TypeArray a')
      TypeOptional a -> go m a >>= \(m, a') -> return (m, TypeOptional a')
      TypeCast a -> go m a >>= \(m, a') -> return (m, TypeCast a')
      TypeTuple [] -> return (m, TypeTuple [])
      TypeTuple as -> do
        (m', as') <-
          foldM
            (\(m', as') a' -> go m' a' >>= \(m'', a'') -> return (m'', a'' : as'))
            (m, [])
            as
        return (m', TypeTuple (reverse as'))
      ty -> return (m, ty)

freshenTypeUnifIds :: Type -> Typing Type
freshenTypeUnifIds = fmap snd . go Map.empty
  where
    go :: Map.Map Unif.Id Unif.Id -> Type -> Typing (Map.Map Unif.Id Unif.Id, Type)
    go m = \case
      TypeUnif u -> case Map.lookup u m of
        Just u' -> return (m, TypeUnif u')
        Nothing -> do
          u' <- freshUnifId (Unif.getLabel u)
          return (Map.insert u u' m, TypeUnif u')
      TypeArray a -> go m a >>= \(m, a') -> return (m, TypeArray a')
      TypeOptional a -> go m a >>= \(m, a') -> return (m, TypeOptional a')
      TypeCast a -> go m a >>= \(m, a') -> return (m, TypeCast a')
      TypeTuple [] -> return (m, TypeTuple [])
      TypeTuple as -> do
        (m', as') <-
          foldM
            (\(m', as') a' -> go m' a' >>= \(m'', a'') -> return (m'', a'' : as'))
            (m, [])
            as
        return (m', TypeTuple (reverse as'))
      ty -> return (m, ty)

-- lookupTermNamed :: Id -> Typing TermNamedValue
-- lookupTermNamed x =
--   ( case tryUnqualify x of
--       Just txt -> asks (^. ctxLocals . at txt)
--       Nothing -> return Nothing
--   )
--     >>= \case
--       Just ty -> return $ TermNamedValueLocal ty
--       Nothing ->
--         gets (^. envModuleCtx . ctxModuleConstants . at x) >>= \case
--           Just con -> return $ TermNamedValueConstant con
--           _ -> throwTypingError $ "unknown term id: " <> prettyShow x

-- data TermNamedValue
--   = TermNamedValueConstant Constant
--   | TermNamedValueLocal Type
