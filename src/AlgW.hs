module AlgW
  ( typeOfExp
  ) where

import AST (Exp)
import AST qualified (Exp(..),Bid(..),Literal(..))
import Ctx (Ctx,typeBool,typeChar,typeString,typeInt,lookupCtx,insertCtx,ctx0,typesFromCtx)
import Infer(Infer(..),IType,instantiate,unify,(-->),getRefine2,mono,generalize,tuple)
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
    arg <- IFresh
    IDebug $ "fresh(" <> pretty x <> "): -> " <> pretty arg
    ret <- typeExp (insertCtx ctx x (mono arg)) body
    pure $ (arg --> ret)
  AST.App e1 _pos e2 -> do
    ret <- IFresh
    IDebug $ "fresh(" <> pretty exp <> "): -> " <> pretty ret
    fun <- typeExp ctx e1
    arg <- typeExp ctx e2
    unify fun (arg --> ret)
    pure ret
  AST.Var _pos x -> do
    instantiate (lookupCtx ctx x)
  AST.Lit _pos  lit ->
    case lit of
      AST.LitN{} -> pure typeInt
      AST.LitC{} -> pure typeChar
      AST.LitS{} -> pure typeString
  AST.RecLam{} -> undefined
  AST.Let _p1 (AST.Bid _p2 x) eRhs eBody -> do
    rhs <- typeExp ctx eRhs
    scheme <- generalize (typesFromCtx ctx) rhs
    typeExp (insertCtx ctx x scheme) eBody
  AST.Tuple es -> do
    ts <- mapM (typeExp ctx) es
    pure $ tuple ts
  AST.Ite e1 e2 e3 -> do
    i <- typeExp ctx e1
    unify i typeBool
    t <- typeExp ctx e2
    e <- typeExp ctx e3
    unify t e
    pure t
