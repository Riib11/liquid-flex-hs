module Language.Flex.Refining where

import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Language.Flex.FlexM (FlexM)
import Language.Flex.Refining.RefiningM
import Language.Flex.Syntax
import Text.PrettyPrint.HughesPJ (Doc)

refineModule :: Module Type -> RefiningM ()
refineModule Module {..} = do
  forM_ moduleDeclarations refineDeclaration

refineDeclaration :: Declaration Type -> RefiningM ()
refineDeclaration = \case
  DeclarationFunction fun -> refineFunction fun
  DeclarationConstant con -> refineConstant con
  _ -> return ()

refineFunction :: Function Type -> RefiningM ()
refineFunction _fun = undefined

refineConstant :: Constant Type -> RefiningM ()
refineConstant _con = undefined
