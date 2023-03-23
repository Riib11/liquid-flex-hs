{-# HLINT ignore "Use ++" #-}
module Language.Flex.Refining.Query where

import Control.Lens
import Control.Monad.Reader (MonadIO (liftIO), MonadTrans (lift), asks, forM, void, when)
import qualified Data.Map as Map
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import Language.Flex.FlexM (FlexCtx (FlexCtx, flexSourceFilePath), defaultSourcePos)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Prelude (preludeDataDecls)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax (SymId (symIdSymbol), Term (termAnn), termVar)
import Language.Flex.Refining.Translating (eqPred, structureDataDecl)
import Language.Flex.Refining.Types
import Text.PrettyPrint.HughesPJ

-- TODO: add primitives and constructor via wrapping Cstr with `H.Any (<Constr>
-- )`
-- TODO: as `qualifiers` argument, give all local bindings
makeQuery :: Cstr -> RefiningM Query
makeQuery cstr = do
  -- TODO: this is too big to log
  -- FlexM.mark [FlexM.FlexMarkStep "makeQuery" . Just $ F.pprint cstr]
  FlexM.mark [FlexM.FlexMarkStep "makeQuery" Nothing]
  -- TODO: qualifiers should only include top-level stuff (constants?)
  qualifiers :: [F.Qualifier] <- do
    asks (^. ctxBindings) >>= \bindings ->
      forM (Map.toList bindings) \(symId, tm) -> do
        let tm' = void <$> tm
        -- p: x == tm
        p <- liftFlex $ eqPred (termVar symId (termAnn tm')) tm'
        pos <- liftFlex defaultSourcePos
        return
          F.Q
            { qName = symIdSymbol symId,
              qParams = [],
              qBody = p,
              qPos = pos
            }

  datadecls :: [F.DataDecl] <- do
    structDataDecls <-
      asks (^. ctxStructures) >>= \structs -> do
        forM
          (Map.elems structs)
          (liftFlex . structureDataDecl)
    return $
      concat
        [ structDataDecls,
          preludeDataDecls
        ]

  FlexM.debugMark False . FlexM.FlexMarkStep "quantifiers" . Just . vcat $
    F.pprint <$> qualifiers

  -- TODO: should include transforms
  -- uninterpreteds <- -- :: HashMap Symbol Sort
  --   return mempty
  let uninterpreteds = mempty

  return $
    H.Query
      qualifiers -- qualifiers (F.Qualifier)
      mempty -- kvars, with parameter-sorts
      cstr -- list of constraints (Cstr)
      uninterpreteds -- list of constants (uninterpreted functions)
      mempty -- list of constants (uninterpreted functions) -- TODO: how is this different from previous??
      mempty -- list of equations (F.Equation)
      mempty -- list of match-es (F.Rewrite)
      datadecls -- list of data declarations (F.DataDecl)

-- | Submit query to LH backend, which checks for validity
submitQuery :: Query -> RefiningM Result
submitQuery q = do
  fp <- liftFlex . asks $ flexSourceFilePath
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