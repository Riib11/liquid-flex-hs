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
import Language.Flex.FlexM (FlexOptions (FlexOptions, sourceFilePath), defaultSourcePos)
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Refining.Prelude (preludeDataDecls)
import Language.Flex.Refining.RefiningM
import Language.Flex.Refining.Syntax (SymId (symIdSymbol), Term (termAnn), termVar)
import Language.Flex.Refining.Translating (eqPred)
import Language.Flex.Refining.Types
import Text.PrettyPrint.HughesPJ

-- TODO: add primitives and constructor via wrapping Cstr with `H.Any (<Constr>
-- )`
-- TODO: as `qualifiers` argument, give all local bindings
makeQuery :: Cstr -> RefiningM Query
makeQuery cstr = do
  -- WARNING: this can be a very big debug
  -- FlexM.tell
  --   $ FlexM.FlexLog
  --     "refining"
  --   $ "makeQuery.cstr:" <+> F.pprint cstr

  -- TODO: qualifiers should only include top-level stuff (constants?)
  qualifiers :: [F.Qualifier] <- return mempty
  -- qualifiers :: [F.Qualifier] <- do
  --   asks (^. ctxBindings) >>= \bindings ->
  --     forM (Map.toList bindings) \(symId, tm) -> do
  --       let tm' = void <$> tm
  --       -- x == tm
  --       p <- eqPred (termVar symId (termAnn tm')) tm'
  --       pos <- lift . lift $ defaultSourcePos
  --       return
  --         F.Q
  --           { qName = symIdSymbol symId,
  --             qParams = [],
  --             qBody = p,
  --             qPos = pos
  --           }

  -- FlexM.tell
  --   $ FlexM.FlexLog
  --     "refining"
  --   $ "makeQuery.quantifiers:"
  --     $$ if null qualifiers
  --       then "EMPTY"
  --       else nest 2 (vcat $ F.pprint <$> qualifiers)

  -- TODO: should include transforms
  uninterpreteds <- -- :: HashMap Symbol Sort
    return mempty

  return $
    H.Query
      qualifiers -- qualifiers (F.Qualifier)
      mempty -- kvars, with parameter-sorts
      cstr -- list of constraints (Cstr)
      uninterpreteds -- list of constants (uninterpreted functions)
      mempty -- list of constants (uninterpreted functions) -- TODO: how is this different from previous??
      mempty -- list of equations (F.Equation)
      mempty -- list of match-es (F.Rewrite)
      preludeDataDecls -- list of data declarations (F.DataDecl)

-- | Submit query to LH backend, which checks for validity
submitQuery :: Query -> RefiningM Result
submitQuery q = do
  fp <- liftFlexM . asks $ sourceFilePath
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