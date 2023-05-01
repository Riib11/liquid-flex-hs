module Language.Flex.Elaboration where

import Control.Lens hiding (enum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Language.Flex.FlexM
import qualified Language.Flex.FlexM as FlexM
import Language.Flex.Syntax as Syntax
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Utility
import Prelude hiding (Enum)

-- extends structures
elaborateModule :: Module Type () -> FlexM (Module Type ())
elaborateModule mdl@Module {..} = do
  -- collect all structures
  let structs :: Map.Map TypeId (Structure Type)
      structs = for moduleDeclarations mempty \case
        (DeclarationStructure struct@Structure {..}) -> Map.insert structureId struct
        _ -> id

  -- collect all refined types
  let refinedTypes :: Map.Map TypeId (RefinedType ())
      refinedTypes = for moduleDeclarations mempty \case
        DeclarationRefinedType rt@RefinedType {..} -> Map.insert refinedTypeId rt
        _ -> id

  let -- fully extend the structure's fields
      extendStructure structIds struct@Structure {..} = FlexM.markSection [FlexM.FlexMarkStep ("extendStructure" <+> pPrint structureId) . Just $ pPrint (structIds, structureId)] do
        -- assert acyclic extensions
        when (structureId `elem` structIds) $ FlexM.throw $ "the structure" <+> ticks (pPrint structureId) <+> "has an extension that forms a cycle:" <+> hcat (punctuate (space <> "extends" <> space) $ pPrint <$> reverse structIds) <+> "in:" <+> pPrint struct

        case (structs Map.!?) =<< structureMaybeExtensionId of
          Nothing -> return struct
          Just struct' -> do
            -- extend the extending struct
            struct'' <- extendStructure structIds struct'

            -- assert non-overlapping fields from extension
            case filter (isJust . (`lookup` Syntax.structureFields struct'') . fst) structureFields of
              interFields | not (null interFields) -> FlexM.throw $ "the structure" <+> ticks (pPrint structureId) <+> "inherits fields that overlap with fields it already has:" <+> pPrint (fst <$> interFields) <+> "in:" <+> pPrint struct
              _ -> return ()

            -- append extending struct's fields to extended struct's fields
            let fields = structureFields <> Syntax.structureFields struct''
            FlexM.debug True $ "extended fields for " <> pPrint structureId <> ": " <> pPrint fields

            return struct {structureFields = fields}

  let -- fully extend the refined type's refinements (if it is refining a structure that extends another structure)
      extendRefinedType tyIds rt@RefinedType {..} = FlexM.markSection [FlexM.FlexMarkStep ("extendRefinedType" <+> pPrint refinedTypeId) . Just $ pPrint (tyIds, refinedTypeId)] case structs Map.!? refinedTypeId of
        Nothing -> return rt
        Just Structure {..} ->
          case structureMaybeExtensionId of
            Nothing -> return rt
            Just extendId -> case refinedTypes Map.!? extendId of
              Nothing -> FlexM.throw $ "unknown refinable type id:" <+> ticks (text $ show structureId) $$ "known refined types:" <+> text (show $ Map.keys refinedTypes)
              Just rt' -> do
                -- extend refinement
                rt'' <- extendRefinedType (refinedTypeId : tyIds) rt'
                let refn =
                      andRefinements
                        [ refinedTypeRefinement,
                          Syntax.refinedTypeRefinement rt''
                        ]
                FlexM.debug True $ "extended refinement for " <> pPrint structureId <> ": " <> pPrint refn
                -- conjoin refinements
                return rt {refinedTypeRefinement = refn}

  moduleDeclarations' <-
    moduleDeclarations <&*> \case
      (DeclarationStructure struct) -> DeclarationStructure <$> extendStructure [] struct
      (DeclarationRefinedType rt) -> DeclarationRefinedType <$> extendRefinedType [] rt
      d -> return d

  return
    mdl
      { moduleDeclarations = moduleDeclarations'
      }
