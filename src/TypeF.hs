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

data PrettyContext = Top | LeftOfArrow

prettyF :: (PrettyContext -> a -> String) -> TypeF a -> String
prettyF pret = \case
  TypeCon (TCon "Tuple") typs -> "(" <> intercalate "," (map (pret Top) typs) <> ")"
  TypeCon c [] -> pretty c
  TypeCon c typs -> pretty c <> "(" <> intercalate "," (map (pret Top) typs) <> ")"
  arg :-> res -> pret LeftOfArrow arg <> "->" <> pret Top res

instance Pretty t => Pretty (TypeF t)  where
  pretty = prettyF (\_ -> pretty)

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
    case bound of
      [] -> prettyM Top body
      _:_ ->
        "forall " <> intercalate " " (map resolve bound) <> ". " <>
        prettyM Top body

    where
      names =
        [ [c] | c <- ['a'..'z'] ]
        <> [ "t" <> show i | i <- [27::Int ..] ]

      TypeScheme {bound,body} = scheme
      mapping = Map.fromList (zip bound names)

      resolve :: TVar -> String
      resolve tv = maybe err id $ Map.lookup tv mapping
        where err = error (show ("pretty/tscheme/resolve",tv))

      prettyM :: PrettyContext -> MType -> String
      prettyM context = \case
        MTypeFix t -> bracket context t (prettyF prettyM t)
        MTypeVar v -> resolve v

      bracket :: PrettyContext -> TypeF MType -> String -> String
      bracket = \case
        Top -> \_ -> \s->s
        LeftOfArrow -> \case
          (_ :-> _) -> \s -> "(" <> s <> ")"
          _ -> \s->s
