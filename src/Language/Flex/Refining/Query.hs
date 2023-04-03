{-# HLINT ignore "Use ++" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Language.Flex.Refining.Query where

import Control.Lens
import Control.Monad
import Control.Monad.Reader (MonadIO (liftIO), MonadTrans (lift), asks)
import Control.Monad.State (StateT (StateT, runStateT), execStateT, modify)
import Data.Bifunctor
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Primitive (primitiveDataDecls)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Reflecting
import Language.Flex.Refining.Syntax
import Language.Flex.Refining.Translating
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

-- - !TODO add primitives and constructor via wrapping Cstr with `H.Any
--   (<Constr> )`
-- - !TODO  as `qualifiers` argument, give all local bindings (?)
-- - !TODO can move definition of datatypes and other top-level stuff to be
--   computed at top level instead of here (which is called every time something
--   is checked)
makeQuery :: Cstr -> RefiningM Query
-- WARNING: this is _very_ long debug if you turn on printing result
makeQuery cstr = FlexM.markSection [FlexM.FlexMarkStep "makeQuery" Nothing] do
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
    -- !TODO add prelude signature (via @applicantTypes@); this will have to
    -- introduce (prelude) functions, but they'll have totally unrefined function types so
    -- don't worry

    -- !TODO things are introduced in @applicantTypes@ now, and all neutral
    -- forms are reflected

    -- intro datatypes
    -- - intro primitive datatypes
    -- - intro user datatypes
    modify $ _qData %~ (primitiveDataDecls <>)
    introUserDatatypes

    -- intro transforms (as uninterpreted functions)
    introTransforms

    -- intro constants (as equations)
    asks (^. ctxConstants . to Map.toList) >>= traverse_ \(tmId, tm) -> do
      let sym = F.symbol tmId
      ex <- lift $ reflTerm tm
      srt <- lift $ reflType (termType tm)
      modify $
        _qEqns
          %~ ( F.Equ
                 { eqName = sym,
                   eqArgs = [],
                   eqBody = ex,
                   eqSort = srt,
                   eqRec = False
                 }
                 :
             )

    -- - !TODO handle qualifiers
    -- - !TODO have to do anything special with qMats (rewrites) for pattern
    --   matching?

    return ()

introUserDatatypes :: StateT Query RefiningM ()
introUserDatatypes = do
  -- add structures
  structs <- asks (^. ctxStructures)
  forM_ (Map.elems structs) \struct -> do
    dd <- lift $ reflStructure struct
    modify $ _qData %~ (dd :)
  -- TODO: add member accessor functions

  -- add variants
  varnts <- asks (^. ctxVariants)
  forM_ (Map.elems varnts) \varnt -> do
    dd <- lift $ reflVariant varnt
    modify $ _qData %~ (dd :)

introTransforms :: StateT Query RefiningM ()
introTransforms = do
  -- !TODO add transforms as uninterpreted functions
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