module TypeF
  ( TypeF(..)
  , TCon(..)
  , TVar(..)
  , MType(..)
  , TypeScheme
  , mkScheme
  ) where

import Data.List (intercalate,nub) -- quadratic nub
import Pretty (Pretty(..))

data TypeF t
  = TypeCon TCon [t]
  | TypeVar TVar -- NICK: maybe this shouldn't be here, but in MType
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

data MType
  = MTypeFix (TypeF MType)

instance Pretty MType where
  pretty = \case
    MTypeFix t -> pretty t

data TypeScheme = TypeScheme
  { bound :: [TVar]
  , body :: MType
  }

mkScheme :: MType -> TypeScheme
mkScheme t = TypeScheme { bound = collectTVars t, body = t }

collectTVars :: MType -> [TVar]
collectTVars = nub . collect []
  where
    collect acc = \case
      MTypeFix (TypeVar v) -> v:acc
      MTypeFix (TypeCon _ ts) -> collects acc ts
      MTypeFix (arg :-> res) -> collect (collect acc res) arg
    collects acc = \case
      [] -> acc
      t1:ts -> collect (collects acc ts) t1

instance Pretty TypeScheme where
  pretty = \TypeScheme {bound=_xs,body} ->
    "forall " <> intercalate " " (map pretty _xs) <> ". " <>
    pretty body
