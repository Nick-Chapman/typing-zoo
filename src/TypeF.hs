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
import Data.Map qualified as Map

data TypeF t
  = TypeCon TCon [t]
  | t :-> t
  deriving (Foldable, Functor)

prettyF :: (a -> String) -> TypeF a -> String
prettyF pret = \case
  TypeCon (TCon "Tuple") typs -> "(" <> intercalate "," (map pret typs) <> ")"
  TypeCon c [] -> pretty c
  TypeCon c typs -> pretty c <> "(" <> intercalate "," (map pret typs) <> ")"
  arg :-> res -> "(" <> pret arg <> "->" <> pret res <> ")"

instance Pretty t => Pretty (TypeF t)  where
  pretty = prettyF pretty

newtype TCon = TCon String deriving (Eq)
instance Pretty TCon where pretty (TCon s) = s

data MType -- M for Mono
  = MTypeFix (TypeF MType)
  | MTypeVar TVar

newtype TVar = TVar { unTVar :: Int } deriving (Eq,Ord,Show)

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
      MTypeVar v -> v:acc
      MTypeFix (TypeCon _ ts) -> collects acc ts
      MTypeFix (arg :-> res) -> collect (collect acc res) arg
    collects acc = \case
      [] -> acc
      t1:ts -> collect (collects acc ts) t1

instance Pretty TypeScheme where
  pretty scheme =
    "forall " <> intercalate " " (map resolve bound) <> ". " <>
    prettyM body

    where
      names =
        [ [c] | c <- ['a'..'z'] ]
        <> [ "t" <> show i | i <- [27::Int ..] ]

      TypeScheme {bound,body} = scheme
      mapping = Map.fromList (zip bound names)

      resolve :: TVar -> String
      resolve tv = maybe err id $ Map.lookup tv mapping
        where err = error (show ("pretty/tscheme/resolve",tv))

      --resolve (TVar u) = show u

      prettyM :: MType -> String
      prettyM = \case
        MTypeFix t -> prettyF prettyM t
        MTypeVar v -> resolve v
