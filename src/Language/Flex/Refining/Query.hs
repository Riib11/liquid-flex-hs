{-# HLINT ignore "Use ++" #-}
module Language.Flex.Refining.Query where

import Control.Lens
import Control.Monad
import Control.Monad.Reader (MonadIO (liftIO), MonadTrans (lift), asks)
import Control.Monad.State (StateT (StateT, runStateT), execStateT, modify)
import Data.Bifunctor
import qualified Data.Map as Map
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Prelude (preludeDataDecls)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating
import Language.Flex.Refining.Types
import qualified Language.Flex.Syntax as Base
import Text.PrettyPrint.HughesPJ hiding (first, (<>))

_qQuals :: Functor f => (_ -> f _) -> _ -> f _
_qQuals = lens H.qQuals (\q qQuals -> q {H.qQuals = qQuals})

_qVars :: Functor f => (_ -> f _) -> _ -> f _
_qVars = lens H.qVars (\q qVars -> q {H.qVars = qVars})

_qCstr :: Functor f => (_ -> f _) -> _ -> f _
_qCstr = lens H.qCstr (\q qCstr -> q {H.qCstr = qCstr})

_qCon :: Functor f => (_ -> f _) -> _ -> f _
_qCon = lens H.qCon (\q qCon -> q {H.qCon = qCon})

_qDis :: Functor f => (_ -> f _) -> _ -> f _
_qDis = lens H.qDis (\q qDis -> q {H.qDis = qDis})

_qEqns :: Functor f => (_ -> f _) -> _ -> f _
_qEqns = lens H.qEqns (\q qEqns -> q {H.qEqns = qEqns})

_qMats :: Functor f => (_ -> f _) -> _ -> f _
_qMats = lens H.qMats (\q qMats -> q {H.qMats = qMats})

_qData :: Functor f => (_ -> f _) -> _ -> f _
_qData = lens H.qData (\q qData -> q {H.qData = qData})

-- - TODO: add primitives and constructor via wrapping Cstr with `H.Any
--   (<Constr> )`
-- - TODO:  as `qualifiers` argument, give all local bindings (?)
-- - TODO: can move definition of datatypes and other top-level stuff to be
--   computed at top level instead of here (which is called every time something
--   is checked)
makeQuery :: Cstr -> RefiningM Query
makeQuery cstr = do
  -- WARNING: this is _very_ long log
  -- FlexM.mark [FlexM.FlexMarkStep "makeQuery" . Just $ F.pprint cstr]
  FlexM.mark [FlexM.FlexMarkStep "makeQuery" Nothing]

  let protoQuery =
        H.Query
          { qQuals = mempty,
            qVars = mempty,
            qCstr = cstr,
            qCon = mempty,
            qDis = mempty,
            qEqns = mempty,
            qMats = mempty,
            qData = mempty
          }

  flip execStateT protoQuery do
    -- add local bindings
    asks (^. ctxBindings) >>= \bindings ->
      forM_ (Map.toList bindings) \(symId, tm) -> do
        let tm' = void <$> tm
        -- p: x == tm
        p <- FlexM.liftFlex $ eqPred (varTerm symId (termAnn tm')) tm'
        pos <- FlexM.liftFlex FlexM.defaultSourcePos
        modify $
          _qQuals
            %~ ( F.Q
                   { qName = symIdSymbol symId,
                     qParams = [],
                     qBody = p,
                     qPos = pos
                   }
                   :
               )

    -- add datatypes
    modify $ _qData %~ (preludeDataDecls <>) -- prelude
    addUserDatatypes -- user datatypes

    -- add transforms
    addTransforms

addUserDatatypes :: StateT Query RefiningM ()
addUserDatatypes = do
  -- add structures
  structs <- asks (^. ctxStructures)
  forM_ (Map.elems structs) \struct@Base.Structure {..} -> do
    -- add DataDecl

    datadecl <- FlexM.liftFlex $ structureDataDecl struct
    modify $ _qData %~ (datadecl :)

{-
-- TODO: maybe actually DataDecl already introduces projectors?

-- add field projectors

fieldSymIds <- forM structureFields $ lift . freshSymIdTermId . Base.fromFieldIdToTermId . fst
fieldTypeRefts <- forM structureFields $ transType . snd
fieldSorts <- forM fieldTypeRefts embedType
let fieldTerms = uncurry varTerm <$> fieldSymIds `zip` fieldTypeRefts

forM_
  ([0 :: Int ..] `zip` structureFields `zip` fieldTypeRefts)
  \((fdIx, (fdId, _fdType)), fdTypeReft) -> do
    -- `F.vv_` is the default refinement bind
    let resultSymId = fromSymbolToSymId F.vv_
    let resultTerm = varTerm resultSymId (void fdTypeReft)

    projSym <- F.val <$> structureFieldProjectorSymbol structureId fdId
    projPos <- FlexM.defaultSourcePos

    structParamSymId <- lift $ freshSymId "struct"
    structParamQParam <- do
      srt <- embedType $ TypeStructure struct ()
      return F.QP {qpSym = symIdSymbol structParamSymId, qpPat = F.PatNone, qpSort = srt}

    constrSymId <- fromSymbolToSymId . F.val <$> structureConstructorSymbol structureId

    let args =
          ([0 ..] `zip` fieldTerms) <&> \(i, fdTerm') ->
            if i == fdIx
              then resultTerm
              else void <$> fdTerm'

    -- p: exists x1 ... x[i-1] x[i+1] ... xn . structParam == S x1 ...
    --    x[i-1] result x[i+1] ... xn
    p <-
      F.pExist
        ((fieldSymIds `zip` fieldSorts) <&> first symIdSymbol)
        <$> eqPred
          (varTerm structParamSymId $ void fdTypeReft)
          (TermNeutral constrSymId args $ void fdTypeReft)

    modify $
      _qQuals
        %~ ( F.Q
               { qName = projSym,
                 qParams = [structParamQParam],
                 qBody = p,
                 qPos = projPos
               }
               :
           )
-}

addTransforms :: StateT Query RefiningM ()
addTransforms = do
  -- TODO: add transforms as uninterpreted functions
  return ()

-- | Submit query to LH backend, which checks for validity
submitQuery :: Query -> RefiningM Result
submitQuery q = do
  fp <- FlexM.liftFlex . asks $ FlexM.flexSourceFilePath
  liftIO (checkValidWithConfig fp fpConfig q)

fpConfig :: FC.Config
fpConfig =
  FC.defConfig
    { FC.eliminate = FC.Some
    }

checkValidWithConfig :: FilePath -> FC.Config -> Query -> IO Result
checkValidWithConfig fp config query = do
  dumpQuery fp query
  fmap snd . F.resStatus <$> HS.solve config query

dumpQuery :: FilePath -> Query -> IO ()
dumpQuery fp query = when True do
  let smtFile = Files.extFileName Files.Smt2 fp
  Misc.ensurePath smtFile
  -- this is not presented to the user, so can use PJ.render
  writeFile smtFile (render . F.pprint $ query)