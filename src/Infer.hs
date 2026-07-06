module Infer
  ( Type(..)
  , TypeError
  , Infer(..)
  , unify
  , getRefine
  , runInfer
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Data.Map qualified as Map
import Pretty (Pretty(..))
import TypeF (TypeF(..))

instance Functor Infer where fmap = liftM
instance Applicative Infer where pure = IPure; (<*>) = ap
instance Monad Infer where (>>=) = IBind

unify :: Type -> Type -> Infer ()
unify ty1 ty2 = do
  ty1 <- refine ty1
  ty2 <- refine ty2
  IDebug ("unify: " <> pretty ty1 <> " ~ " <> pretty ty2)
  let mismatch = IFail (pretty ty1 <> " ~ " <> pretty ty2)
  case (ty1,ty2) of
    (ty, TypeUnknown v) -> subTy v ty
    (TypeUnknown v, ty) -> subTy v ty
    (Type (TypeCon c1 typs1), Type (TypeCon c2 typs2)) | c1==c2 -> do
      if length typs1 /= length typs2 then mismatch else
        sequence_ [ unify ty1 ty2 | (ty1,ty2) <- zip typs1 typs2 ]
    (Type (TypeCon{}), _) -> mismatch
    (_, Type (TypeCon{})) -> mismatch
    (Type (a :-> b), Type (c :-> d)) -> do
      unify a c
      unify b d

refine :: Type -> Infer Type
refine ty = do f <- getRefine; pure (f ty)

getRefine :: Infer (Type -> Type)
getRefine = do
  subst <- ICurrentSubst
  pure (refineTypeWithSubst subst)

subTy :: UniVar -> Type -> Infer ()
subTy v ty = if v `occurs` ty then IFail "occurs" else do
  IDebug $ "sub: " <> pretty v <> " := " <> pretty ty
  ISub v ty

data Infer a where
  IPure :: a -> Infer a
  IBind :: Infer a -> (a -> Infer b) -> Infer b
  IFresh :: Infer UniVar
  ISub :: UniVar -> Type -> Infer ()
  IFail :: String -> Infer ()
  ICurrentSubst :: Infer Subst
  IDebug :: String -> Infer ()

type IRes a = Either TypeError a

runInfer :: Infer a -> IO (IRes a)
runInfer infer = loop state0 infer \_s a -> pure (Right a)
  where
    loop :: IState -> Infer a -> (IState -> a -> IO (IRes b)) -> IO (IRes b)
    loop s = \case
      IPure a -> \k -> k s a
      IBind m g -> \k -> loop s m \s a -> loop s (g a) k
      IDebug _mes -> \k -> do
        --putStrLn _mes
        k s ()
      IFresh -> \k -> do
        let IState{u} = s
        let var = UniVar u
        k s { u = u + 1 } var
      ISub v ty -> \k -> do
        let IState{subst=subst0} = s
        let subst = subExtend subst0 v ty
        k s { subst } ()
      IFail mes -> \_k -> do
        pure (Left (TypeError mes))
      ICurrentSubst -> \k -> do
        let IState{subst} = s
        k s subst

data IState = IState { u :: Int, subst :: Subst }
state0 :: IState
state0 = IState { u = 0, subst = subst0 }

data Subst = Subst { vmap :: Map UniVar Type }
instance Pretty Subst where pretty Subst{vmap=m} = pretty m

subst0 :: Subst
subst0 = Subst { vmap = Map.empty }

subExtend :: Subst -> UniVar -> Type -> Subst
subExtend subst v ty = do
  let Subst{vmap = vmap0} = subst
  let ty' = refineTypeWithSubst subst ty
  let f v' = if v == v' then Just ty' else Nothing
  let shifted = Map.map (specialize f) vmap0
  let vmap = Map.insert v ty' shifted
  Subst {vmap}

data Type
  = Type (TypeF Type)
  | TypeUnknown UniVar

instance Pretty Type where
  pretty = \case
    TypeUnknown v -> pretty v
    Type t -> pretty t

occurs :: UniVar -> Type -> Bool
occurs v = loop
  where
    loop = \case
      TypeUnknown v' -> v == v'
      Type t -> any loop t

refineTypeWithSubst :: Subst -> Type -> Type
refineTypeWithSubst Subst{vmap} ty = specialize (\v -> Map.lookup v vmap) ty

specialize :: (UniVar -> Maybe Type) -> Type -> Type
specialize f = trav
  where
    trav = \case
      ty@(TypeUnknown v) -> case f v of Just ty' -> ty'; Nothing -> ty
      Type t -> Type (fmap trav t)

newtype UniVar = UniVar { unUniVar :: Int } deriving (Eq,Ord,Show)
instance Pretty UniVar where pretty (UniVar i) = show i

data TypeError = TypeError String deriving Show
instance Pretty TypeError where pretty (TypeError s) = s
