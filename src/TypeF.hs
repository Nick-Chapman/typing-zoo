module TypeF
  ( TypeF(..)
  , TCon(..)
  , TVar(..)
  , FixType(..)
  ) where

import Data.List (intercalate)
import Pretty (Pretty(..))

data TypeF t
  = TypeCon TCon [t]
  | TypeVar TVar
  | t :-> t
  deriving (Foldable, Functor)

instance Pretty t => Pretty (TypeF t)  where
  pretty = \case
    TypeCon (TCon "Tuple") typs -> "(" <> intercalate "," (map pretty typs) <> ")"
    TypeVar v -> pretty v
    TypeCon c [] -> pretty c
    TypeCon c typs -> pretty c <> "(" <> intercalate "," (map pretty typs) <> ")"
    arg :-> res -> "(" <> pretty arg <> "->" <> pretty res <> ")"

newtype TCon = TCon String deriving (Eq)
instance Pretty TCon where pretty (TCon s) = s

newtype TVar = TVar { unTVar :: Int } deriving (Eq,Ord,Show)
instance Pretty TVar where pretty (TVar i) = "" <> show i

data FixType = FixType (TypeF FixType)

instance Pretty FixType where
  pretty = \case
    FixType t -> pretty t
