module AlgM
  ( typeOfExp
  ) where

import AST (Exp)
import AST qualified (Exp(..),Bid(..),Literal(..))
import Ctx (Ctx,typeBool,typeChar,typeString,typeInt,lookupCtx,insertCtx,ctx0,typesFromCtx)
import Infer (Infer(..),IType,instantiate,unify,(-->),getRefine2,mono,generalize,tuple)
import TypeF (TypeScheme)

typeOfExp :: Exp -> Infer TypeScheme
typeOfExp exp = do
  t <- IFresh
  typeExpAs ctx0 exp t
  refine <- getRefine2
  pure (refine t)

typeExpAs :: Ctx -> Exp -> IType -> Infer ()
typeExpAs ctx exp expected = case exp of
  AST.Lam _pos (AST.Bid _ x) body -> do
    arg <- IFresh
    res <- IFresh
    let got = (arg --> res)
    unify expected got
    typeExpAs (insertCtx ctx x (mono arg)) body res
  AST.App e1 _pos e2 -> do
    arg <- IFresh
    -- which order is best?
    typeExpAs ctx e1 (arg --> expected)
    typeExpAs ctx e2 arg

  AST.Var _pos x -> do
    got <- instantiate (lookupCtx ctx x)
    unify expected got
  AST.Lit _pos lit -> do
    let got =
          case lit of
            AST.LitN{} -> typeInt
            AST.LitC{} -> typeChar
            AST.LitS{} -> typeString
    unify expected got
  AST.RecLam{} -> undefined
  AST.Let _p1 (AST.Bid _p2 x) eRhs eBody -> do
    rhs <- IFresh
    typeExpAs ctx eRhs rhs
    scheme <- generalize (typesFromCtx ctx) rhs
    typeExpAs (insertCtx ctx x scheme) eBody expected
  AST.Tuple es -> do
    pairs <- sequence [ do t <- IFresh; pure (e,t) | e <- es ]
    let got = tuple (map snd pairs)
    unify expected got
    sequence_ [ typeExpAs ctx e t | (e,t) <- pairs ]
  AST.Ite i t e -> do
    typeExpAs ctx i typeBool
    typeExpAs ctx t expected
    typeExpAs ctx e expected
