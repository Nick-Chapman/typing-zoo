module Infer
  ( IType
  , typeBase0
  , tuple
  , (-->)
  , Infer(..)
  , unify
  , getRefine2
  , runInfer
  , TypeError
  , ITypeScheme
  , mono
  , generalize
  , instantiate
  ) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Data.Map qualified as Map
import Pretty (Pretty(..))
import TypeF (TypeF(..),TCon(..),TVar(..),MType(..),TypeScheme,mkScheme)
import Data.List (intercalate,nub, (\\)) -- quadratic nub

instance Functor Infer where fmap = liftM
instance Applicative Infer where pure = IPure; (<*>) = ap
instance Monad Infer where (>>=) = IBind

typeBase0 :: String -> IType
typeBase0 name = ITypeFix (TypeCon (TCon name) [])

tuple :: [IType] -> IType
tuple ts = ITypeFix (TypeCon (TCon "Tuple") ts)

(-->) :: IType -> IType -> IType
(-->) a b = ITypeFix (a :-> b)

data IType
  = ITypeFix (TypeF IType)
  | ITypeUnknown UniVar

instance Pretty IType where
  pretty = \case
    ITypeUnknown v -> pretty v
    ITypeFix t -> pretty t

newtype UniVar = UniVar { unUniVar :: Int } deriving (Eq,Ord,Show)
instance Pretty UniVar where pretty (UniVar i) = show i

unify :: IType -> IType -> Infer ()
unify ty1 ty2 = do
  ty1 <- refine ty1
  ty2 <- refine ty2
  IDebug ("unify: " <> pretty ty1 <> " ~ " <> pretty ty2)
  let mismatch = IFail (pretty ty1 <> " ~ " <> pretty ty2)
  case (ty1,ty2) of
    (ty, ITypeUnknown v) -> subTy v ty
    (ITypeUnknown v, ty) -> subTy v ty
    (ITypeFix (TypeCon c1 typs1), ITypeFix (TypeCon c2 typs2)) | c1==c2 -> do
      if length typs1 /= length typs2 then mismatch else
        sequence_ [ unify ty1 ty2 | (ty1,ty2) <- zip typs1 typs2 ]
    (ITypeFix (TypeCon{}), _) -> mismatch
    (_, ITypeFix (TypeCon{})) -> mismatch
    (ITypeFix (a :-> b), ITypeFix (c :-> d)) -> do
      unify a c
      unify b d

refine :: IType -> Infer IType
refine ty = do f <- getRefine1; pure (f ty)

subTy :: UniVar -> IType -> Infer ()
subTy v ty = if v `occurs` ty then IFail "occurs" else do
  IDebug $ "sub: " <> pretty v <> " := " <> pretty ty
  ISub v ty

occurs :: UniVar -> IType -> Bool
occurs v = loop
  where
    loop = \case
      ITypeUnknown v' -> v == v'
      ITypeFix t -> any loop t

getRefine1 :: Infer (IType -> IType)
getRefine1 = do
  subst <- ICurrentSubst
  pure (refineTypeWithSubst subst)

getRefine2 :: Infer (IType -> TypeScheme)
getRefine2 = (generalizeType . ) <$>  getRefine1

data Infer a where
  IPure :: a -> Infer a
  IBind :: Infer a -> (a -> Infer b) -> Infer b
  IFresh :: Infer IType
  ISub :: UniVar -> IType -> Infer ()
  IFail :: String -> Infer ()
  ICurrentSubst :: Infer Subst
  IDebug :: String -> Infer ()

type IRes a = Either TypeError a

data TypeError = TypeError String deriving Show
instance Pretty TypeError where pretty (TypeError s) = s

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
        k s { u = u + 1 } (ITypeUnknown var)
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

data Subst = Subst { vmap :: Map UniVar IType }
instance Pretty Subst where pretty Subst{vmap=m} = pretty m

subst0 :: Subst
subst0 = Subst { vmap = Map.empty }

subExtend :: Subst -> UniVar -> IType -> Subst
subExtend subst v ty = do
  let Subst{vmap = vmap0} = subst
  let ty' = refineTypeWithSubst subst ty
  let f v' = if v == v' then Just ty' else Nothing
  let shifted = Map.map (specialize f) vmap0
  let vmap = Map.insert v ty' shifted
  Subst {vmap}

refineTypeWithSubst :: Subst -> IType -> IType
refineTypeWithSubst Subst{vmap} ty = specialize (\v -> Map.lookup v vmap) ty

specialize :: (UniVar -> Maybe IType) -> IType -> IType
specialize f = trav
  where
    trav = \case
      ty@(ITypeUnknown v) -> case f v of Just ty' -> ty'; Nothing -> ty
      ITypeFix t -> ITypeFix (fmap trav t)

generalizeType :: IType -> TypeScheme
generalizeType = mkScheme . trav
  where
    trav = \case
      ITypeUnknown (UniVar u) -> MTypeVar (TVar u)
      ITypeFix t -> MTypeFix (fmap trav t)

----------------------------------------------------------------------

data ITypeScheme = ITypeScheme [UniVar] IType
--newtype GVar = GVar { unGVar :: Int } --deriving (Eq,Ord,Show)
--instance Pretty GVar where pretty (GVar i) = "g" <> show i

instance Pretty ITypeScheme where
  pretty (ITypeScheme vs ty) =
    "forall " <> intercalate " " (map pretty vs) <> "." <> pretty ty

mono :: IType -> ITypeScheme
mono ty = ITypeScheme [] ty

generalize :: [ITypeScheme] -> IType -> Infer ITypeScheme
generalize contextSchemes ty = do
  rty <- refine ty
  let as = collectUniVars [rty]
  cTys <- sequence [ refine ty | ITypeScheme _ ty <- contextSchemes ]
  let bs = collectUniVars cTys
  IDebug ("as: " <> see as)
  IDebug ("bs: " <> see bs)
  let xs = as \\ bs
  let ty' = ITypeScheme xs rty
  IDebug ("gen: " <> pretty rty <> " --> " <> pretty ty')
  pure ty'
    where see vs = intercalate " " (map pretty vs)

instantiate :: ITypeScheme -> Infer IType
instantiate (ITypeScheme xs ty) = do
  m <- Map.fromList <$> sequence [ do y <- IFresh; pure (x,y) | x <- xs ]
  let ty' = specialize (\v -> Map.lookup v m) ty
  pure ty'

collectUniVars :: [IType] -> [UniVar]
collectUniVars = nub . collects []
  where
    collect acc = \case
      ITypeUnknown v -> v:acc
      ITypeFix (TypeCon _ ts) -> collects acc ts
      ITypeFix (arg :-> res) -> collect (collect acc res) arg
    collects acc = \case
      [] -> acc
      t1:ts -> collect (collects acc ts) t1
