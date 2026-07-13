module AlgW
  ( typeOfExp
  ) where

import AST (Exp,Id,mkUserId)
import AST qualified (Exp(..),Bid(..),Literal(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Infer (IType,Infer(..),typeBase0,tuple,unify,(-->),getRefine2,ITypeScheme,generalize,instantiate,mono)
import Pretty (Pretty(..))
import TypeF (TypeScheme)

typeOfExp :: Exp -> Infer TypeScheme
typeOfExp exp = do
  t <- typeExp ctx0 exp
  refine <- getRefine2
  pure (refine t)

typeExp :: Ctx -> Exp -> Infer IType
typeExp ctx exp = case exp of
  AST.Lam _pos (AST.Bid _ x) body -> do
    let Ctx{xmap} = ctx
    arg <- IFresh
    IDebug $ "fresh(" <> pretty x <> "): -> " <> pretty arg
    let ctx1 = ctx { xmap = Map.insert x (mono arg) xmap }
    ret <- typeExp ctx1 body
    pure $ (arg --> ret)
  AST.App e1 _pos e2 -> do
    ret <- IFresh
    IDebug $ "fresh(" <> pretty exp <> "): -> " <> pretty ret
    fun <- typeExp ctx e1
    arg <- typeExp ctx e2
    unify fun (arg --> ret)
    pure ret
  AST.Var _pos x -> do
    let Ctx{xmap} = ctx
    let err = error ("unbound var: '" <> pretty x <> "'")
    let scheme = maybe err id $ Map.lookup x xmap
    instantiate scheme
  AST.Lit _pos  lit ->
    case lit of
      AST.LitN{} -> pure typeInt
      AST.LitC{} -> pure typeChar
      AST.LitS{} -> pure typeString
  AST.RecLam{} -> undefined
  AST.Let _p1 (AST.Bid _p2 x) eRhs eBody -> do
    rhs <- typeExp ctx eRhs
    let Ctx{xmap} = ctx
    let ss :: [ITypeScheme] = [ s | (_,s) <- Map.toList xmap ]
    tScheme <- generalize ss rhs
    let ctx1 = ctx { xmap = Map.insert x tScheme xmap }
    typeExp ctx1 eBody
  AST.Tuple es -> do
    ts <- mapM (typeExp ctx) es
    pure $ tuple ts


data Ctx = Ctx { xmap :: Map Id ITypeScheme }
instance Pretty Ctx where pretty Ctx{xmap=m} = pretty m

ctx0 :: Ctx
ctx0 = Ctx { xmap = Map.fromList [ (mkUserId x, mono ty) | (x,ty) <- init ] }
  where
    init =
      [ ("true", typeBool)
      , ("false", typeBool)
      , ("-", typeInt --> typeInt --> typeInt)
      , ("+", typeInt --> typeInt --> typeInt)
      , ("*", typeInt --> typeInt --> typeInt)
      , ("/", typeInt --> typeInt --> typeInt)
      , ("&&", typeBool --> typeBool --> typeBool)
      , ("||", typeBool --> typeBool --> typeBool)
      , ("not", typeBool --> typeBool)
      ]

typeInt,typeChar,typeString,typeBool :: IType
typeInt = typeBase0 "Int"
typeChar = typeBase0 "Char"
typeString = typeBase0 "String"
typeBool = typeBase0 "Bool"
