module Flex.Refining
  ( module Flex.Refining.Common,
    module Flex.Refining.Check,
    module Flex.Refining.Query,
    module Flex.Refining.Syntax,
    module Flex.Refining.Translation,
    checkModule,
    checkDeclaration,
    checkFunction,
    checkConstant,
  )
where

import Control.DeepSeq
import Control.Exception
import Control.Monad (foldM, void, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (second)
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack, unpack)
import Data.Typeable
import Flex.Flex
import Flex.Refining.Check
import Flex.Refining.Common
import Flex.Refining.Query
import Flex.Refining.Syntax
import Flex.Refining.Translation
import qualified Flex.Syntax as Base
import GHC.Generics
import GHC.IO.Exception (ExitCode)
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Misc as F
import qualified Language.Fixpoint.Misc as Misc
import qualified Language.Fixpoint.Parse as FP
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as Files
import PrettyShow (PrettyShow (prettyShow))
import System.Exit (exitWith)
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Text.Printf (printf)
import Utility

checkModule :: Base.Module -> Refining ()
checkModule = undefined

checkDeclaration :: Base.Declaration -> Refining ()
checkDeclaration = undefined

checkFunction :: Base.Function -> Refining ()
checkFunction = undefined

checkConstant :: Base.Constant -> Refining ()
checkConstant con = do
  k <- case Base.constantBody con of
    Base.DefinitionBodyTerm tm -> return \f -> f tm
    Base.DefinitionBodyDerived (Just tm) -> return \f -> f tm
    _ -> return \_f -> return ()
  k \tm -> do
    tm' <- transTerm tm
    checkValid =<< makeCheckTermQuery tm' (termType tm')
