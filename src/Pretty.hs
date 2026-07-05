module Pretty (Pretty(..)) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.List (intercalate)

class Pretty a where
  pretty :: a -> String

instance (Pretty k,Pretty v) => Pretty (Map k v) where
  pretty m =
    "[" <> intercalate "," [ pretty v <> "~>" <> pretty ty
                           | (v,ty) <- Map.toList m
                           ] <> "]"
