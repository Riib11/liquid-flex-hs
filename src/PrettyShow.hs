module PrettyShow where

import Data.List (intercalate)
import Data.Map as Map
import Data.Text (Text, unpack)

class PrettyShow a where
  prettyShow :: a -> String

instance PrettyShow a => PrettyShow [a] where
  prettyShow as = "[" <> intercalate ", " (prettyShow <$> as) <> "]"

instance (Show k, PrettyShow v) => PrettyShow (Map.Map k v) where
  prettyShow m = "[" <> intercalate ", " ((\(k, v) -> show k <> ": " <> prettyShow v) <$> (Map.toList m)) <> "]"

-- instance (PrettyShow k, PrettyShow v) => PrettyShow (Map.Map k v) where
--   prettyShow m = "[" <> intercalate ", " ((\(k, v) -> prettyShow k <> ": " <> prettyShow v) <$> (Map.toList m)) <> "]"

instance PrettyShow a => PrettyShow (Maybe a) where
  prettyShow = \case
    Nothing -> "<Nothing>"
    Just a -> prettyShow a

instance (PrettyShow a, PrettyShow b) => PrettyShow (Either a b) where
  prettyShow = \case
    Left a -> prettyShow a
    Right b -> prettyShow b

instance PrettyShow Text where
  prettyShow = unpack

indent :: String -> String
indent = unlines . (("  " <>) <$>) . lines
