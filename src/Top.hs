module Top (main) where

import AST (Exp,Id,mkUserId)
import AST qualified (Exp(..),Bid(..),Literal(..))
import Control.Monad (ap,liftM)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString(fromString))
import Parser (parse)
import Pretty (Pretty(..))

main :: IO ()
main = do
  putStrLn "*typing-zoo*"
  -- example 12 has wrong type
  xs <- pick [12] . filterExamples . lines <$> readFile "basic.fun"
  mapM_ runExample xs
    where

      pick ns xs = [ (n, xs!!n) | n <- ns ]
      filterExamples = filter (not . empty) . map dropComment
      dropComment :: String -> String
      dropComment = takeWhile (/= '#')
      empty :: String -> Bool
      empty s = s==""

runExample :: (Int,String) -> IO ()
runExample (i,s) = do
  putStrLn $ "[" <> show i <> "] "
  putStrLn $ "raw: " <> s
  let exp = parse s
  putStrLn $ "exp: " <> pretty exp
  runInferTypeOfExp exp >>= \case
        Left err -> putStrLn ("**type error: " <> pretty err)
        Right ty -> putStrLn ("type: " <> pretty ty)

runInferTypeOfExp :: Exp -> IO (Either TypeError Type)
runInferTypeOfExp exp = do
  runInfer $ do
    typeExp ctx0 exp >>= refine

typeExp :: Ctx -> Exp -> Infer Type
typeExp ctx = \case
  AST.Lam _pos (AST.Bid _ x) body -> do
    let Ctx{xmap} = ctx
    tyArg <- TypeVar <$> IFresh
    let ctx1 = ctx { xmap = Map.insert x tyArg xmap }
    tyRes <- typeExp ctx1 body
    pure (tyArg :-> tyRes)
  AST.App fun _pos arg -> do
    tyRes <- TypeVar <$> IFresh
    tyFun <- typeExp ctx fun
    tyArg <- typeExp ctx arg
    unifyTy tyFun (tyArg :-> tyRes)
    pure tyRes
  AST.Var _pos x -> do
    let Ctx{xmap} = ctx
    let err = error ("typeExp/EVar" <> pretty x)
    pure $ maybe err id $ Map.lookup x xmap
  AST.Lit _pos  lit ->
    case lit of
      AST.LitN{} -> pure TypeInt
      AST.LitC{} -> undefined
      AST.LitS{} -> undefined
  AST.RecLam{} -> do
    undefined
  AST.Let{} -> do
    undefined

data Ctx = Ctx { xmap :: Map Id Type }
ctx0 :: Ctx
ctx0 = Ctx { xmap = Map.fromList [ (mkUserId x, ty) | (x,ty) <- init ] }
  where
    init =
      [ ("true", TypeBool)
      , ("false", TypeBool)
      ]

unifyTy :: Type -> Type -> Infer ()
unifyTy ty1 ty2 = do
  ty1 <- refine ty1
  ty2 <- refine ty2
  unify (ty1,ty2)
  where
    mismatch = undefined
    unify = \case
      (ty, TypeVar v) -> subTy v ty
      (TypeVar v, ty) -> subTy v ty
      (TypeInt, TypeInt) -> pure ()
      (TypeInt, _) -> mismatch
      (_, TypeInt) -> mismatch
      (TypeBool, TypeBool) -> pure ()
      (TypeBool, _) -> mismatch
      (_, TypeBool) -> mismatch
      (a :-> b, c :-> d) -> do
        unify (a,c)
        unify (b,d)

refine :: Type -> Infer Type
refine ty = refineWithSubst ty <$> ICurrentSubst

subTy :: TVar -> Type -> Infer ()
subTy v ty = if v `occurs` ty then IFail "occurs" else ISub v ty

instance Functor Infer where fmap = liftM
instance Applicative Infer where pure = IPure; (<*>) = ap
instance Monad Infer where (>>=) = IBind

data Infer a where
  IPure :: a -> Infer a
  IBind :: Infer a -> (a -> Infer b) -> Infer b
  IFresh :: Infer TVar
  ISub :: TVar -> Type -> Infer ()
  IFail :: String -> Infer ()
  ICurrentSubst :: Infer Subst

type IRes a = Either TypeError a

runInfer :: Infer a -> IO (IRes a)
runInfer infer = loop state0 infer \_s a -> pure (Right a)
  where
    loop :: IState -> Infer a -> (IState -> a -> IO (IRes b)) -> IO (IRes b)
    loop s = \case
      IPure a -> \k -> k s a
      IBind m g -> \k -> loop s m \s a -> loop s (g a) k
      IFresh -> \k -> do
        let IState{u} = s
        let var = TVar ("t" <> show u)
        --putStrLn ("fresh: -> " <> pretty var)
        k s { u = u + 1 } var
      ISub v ty -> \k -> do
        --putStrLn ("sub: " <> pretty v <> "~>" <> pretty ty)
        let IState{subst=subst0} = s
        let subst = subExtend subst0 v ty
        --putStrLn ("subst: " <> pretty subst)
        k s { subst } ()
      IFail mes -> \_k -> do
        pure (Left (TypeError mes))
      ICurrentSubst -> \k -> do
        let IState{subst} = s
        k s subst

data IState = IState { u :: Int, subst :: Subst }
state0 :: IState
state0 = IState { u = 0, subst = subst0 }

data Subst = Subst { vmap :: Map TVar Type }

instance Pretty Subst where
  pretty Subst{vmap} =
    "[" <> intercalate "," [ pretty v <> "~>" <> pretty ty
                           | (v,ty) <- Map.toList vmap
                           ] <> "]"

subst0 :: Subst
subst0 = Subst { vmap = Map.empty }

subExtend :: Subst -> TVar -> Type -> Subst
subExtend subst v ty = do
  let Subst{vmap = vmap0} = subst
  --let domain = Map.keys vmap0
  --if v `elem` domain then error "subExtend" else do
  let ty' = refineWithSubst ty subst
  let f v' = if v == v' then Just ty' else Nothing
  let shifted = Map.map (specialize f) vmap0
  let vmap = Map.insert v ty' shifted
  Subst {vmap}

data Type
  = TypeVar TVar
  | TypeInt
  | TypeBool
  | Type :-> Type
  deriving Show

instance Pretty Type where
  pretty = \case
    TypeVar v -> pretty v
    TypeInt -> "Int"
    TypeBool -> "Bool"
    arg :-> res -> "(" <> pretty arg <> "->" <> pretty res <> ")"

occurs :: TVar -> Type -> Bool
occurs v = loop
  where
    loop = \case
      TypeVar v' -> v == v'
      TypeInt -> False
      TypeBool -> False
      ty1 :-> ty2 -> loop ty1 || loop ty2

refineWithSubst :: Type -> Subst -> Type
refineWithSubst ty Subst{vmap} = specialize (\v -> Map.lookup v vmap) ty

specialize :: (TVar -> Maybe Type) -> Type -> Type
specialize f = trav
  where
    trav = \case
      ty@(TypeVar v) -> case f v of Just ty' -> ty'; Nothing -> ty
      ty@TypeInt -> ty
      ty@TypeBool -> ty
      ty1 :-> ty2 -> trav ty1 :-> trav ty2

newtype TVar = TVar { unTVar :: String } deriving (Eq,Ord,Show)
instance IsString TVar where fromString = TVar
instance Pretty TVar where pretty = unTVar

data TypeError = TypeError String deriving Show
instance Pretty TypeError where pretty (TypeError s) = s
