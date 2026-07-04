
module AST
  ( Exp(..)
  , Literal(..)
  , Bid(..)
  , Id
  , mkUserId
  ) where

import Data.String (IsString(fromString))
import Par4 (Pos)
import Pretty (Pretty(..))

data Exp
  = Var Pos Id
  | Lit Pos Literal
--  | Con Pos Cid [Exp]
--  | Prim Pos Primitive [Exp]
  | Lam Pos Bid Exp
  | RecLam Pos Bool Bid Bid Exp
  | App Exp Pos Exp
  | Let Pos Bid Exp Exp
--  | Match Pos Exp [Arm]

data Bid = Bid Pos Id -- we always know the position of a bound identifier...
instance Pretty Bid where pretty (Bid _ x) = pretty x -- ...but we never show it!

mkUserId :: String -> Id
mkUserId = Id

data Literal = LitC Char | LitN Int | LitS String

instance Pretty Exp where
  pretty = \case
    Lam _ x e -> "(\\" <> pretty x <> "->" <> pretty e <> ")"
    App fun _ arg -> "(" <> pretty fun <> " " <> pretty arg <> ")"
    Var _ x -> pretty x
    Lit _ x -> pretty x
    RecLam{} -> undefined
    Let{} -> undefined

instance Pretty Literal where
  pretty = \case
    LitC c -> show c
    LitN n -> show n
    LitS s -> show s

newtype Id = Id { unId :: String } deriving (Eq,Ord)
instance IsString Id where fromString = Id
instance Pretty Id where pretty = unId
