{-# HLINT ignore "Use ++" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Language.Flex.Refining.Query where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
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
      tm' <- lift $ transTerm tm
      ex <- lift $ reflTerm tm'
      srt <- lift $ reflType (termType tm')
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

    -- - !TODO handle qualifiers (??)
    -- - !TODO have to do anything special with qMats (rewrites) for pattern
    --   matching?

    return ()

introUserDatatypes :: StateT Query RefiningM ()
introUserDatatypes = do
  -- intro structures
  structs <- asks (^. ctxStructures)
  forM_ (Map.elems structs) \struct -> do
    dd <- lift $ reflStructure struct
    modify $ _qData %~ (dd :)
  -- TODO: add member accessor functions

  -- intro variants
  varnts <- asks (^. ctxVariants)
  forM_ (Map.elems varnts) \varnt -> do
    dd <- lift $ reflVariant varnt
    modify $ _qData %~ (dd :)

introTransforms :: StateT Query RefiningM ()
introTransforms = do
  -- intro transforms as uninterpreted functions (in Query.qCon)
  funs <- asks (^. ctxFunctions)
  forM_ (Map.elems funs) \Function {..} ->
    when functionIsTransform do
      funOutputSort <- lift $ reflType functionOutput
      funSort <- lift $ foldr F.FFunc funOutputSort <$> ((reflType . snd) `traverse` functionParameters)
      modify $ _qCon . at (F.symbol functionId) ?~ funSort

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
