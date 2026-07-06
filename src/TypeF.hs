module TypeF
  ( TCon(..)
  , TypeF(..)
  ) where

import Data.List (intercalate)
import Pretty (Pretty(..))

data TypeF t
  = TypeCon TCon [t]
  | t :-> t
  deriving (Foldable, Functor)

instance Pretty t => Pretty (TypeF t)  where
  pretty = \case
    TypeCon (TCon "Tuple") typs -> "(" <> intercalate "," (map pretty typs) <> ")"
    TypeCon c [] -> pretty c
    TypeCon c typs -> pretty c <> "(" <> intercalate "," (map pretty typs) <> ")"
    arg :-> res -> "(" <> pretty arg <> "->" <> pretty res <> ")"

newtype TCon = TCon String deriving (Eq)
instance Pretty TCon where pretty (TCon s) = s
