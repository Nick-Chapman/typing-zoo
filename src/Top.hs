module Top (main) where

import AST (Exp,Id,mkUserId)
import AST qualified (Exp(..),Bid(..),Literal(..))
import Control.Monad (ap,liftM)
import Data.Map (Map)
import Data.Map qualified as Map
import Parser (parse)
import Pretty (Pretty(..))
import TypeF (TypeF(..),TCon(..))

main :: IO ()
main = do
  putStrLn "*typing-zoo*"
  -- example 12 has wrong type
  xs <- zip [0..] . filterExamples . lines <$> readFile "basic.fun"
  mapM_ runExample xs
    where
      _pick ns xs = [ (n, xs!!n) | n <- ns ]
      filterExamples = filter (not . empty) . map dropComment
      dropComment :: String -> String
      dropComment = takeWhile (/= '#')
      empty :: String -> Bool
      empty s = s==""

runExample :: (Int,String) -> IO ()
runExample (i,s) = do
  let exp = parse s
  putStrLn $ "[" <> show i <> "] " <> s
  --putStrLn $ "[" <> show i <> "] " <> pretty exp
  runInferTypeOfExp exp >>= \case
        Left err -> putStrLn ("**type error: " <> pretty err)
        Right (_d@(Derivation (J _ _ ty) _)) -> do
          putStrLn (":: " <> pretty ty)
          --putStrLn ("derivation: " <> pretty _d)

runInferTypeOfExp :: Exp -> IO (Either TypeError Derivation)
runInferTypeOfExp exp = do
  runInfer $ do
    d <- typeExp ctx0 exp
    refineDerivation d

typeExp :: Ctx -> Exp -> Infer Derivation
typeExp ctx exp = case exp of
  AST.Lam _pos (AST.Bid _ x) body -> do
    let Ctx{xmap} = ctx
    typArg <- TypeUnknown <$> IFresh (pretty x)
    let ctx1 = ctx { xmap = Map.insert x typArg xmap }
    d1@(Derivation (J _ _ typRes) _) <- typeExp ctx1 body
    let typFun = Type (typArg :-> typRes)
    pure $ Derivation (J ctx exp typFun) [d1]
  AST.App fun _pos arg -> do
    typRes <- TypeUnknown <$> IFresh (pretty exp)
    d1@(Derivation (J _ _ typFun) _) <- typeExp ctx fun
    d2@(Derivation (J _ _ typArg) _) <- typeExp ctx arg
    unify typFun (Type (typArg :-> typRes))
    pure $ Derivation (J ctx exp typRes) [d1,d2]
  AST.Var _pos x -> do
    let Ctx{xmap} = ctx
    let err = error ("typeExp/EVar" <> pretty x)
    let typ = maybe err id $ Map.lookup x xmap
    pure $ Derivation (J ctx exp typ) []
  AST.Lit _pos  lit ->
    case lit of
      AST.LitN{} -> pure $ Derivation (J ctx exp typeInt) []
      AST.LitC{} -> undefined
      AST.LitS{} -> undefined
  AST.RecLam{} -> undefined
  AST.Let pos x rhs body -> do -- temp; prior to support generalization
    let func = AST.Lam pos x body
    let appliedAbstraction = AST.App func pos rhs
    typeExp ctx appliedAbstraction
  AST.Tuple es -> do
    ds <- mapM (typeExp ctx) es
    let typs = [ typ | Derivation (J _ _ typ) _ <- ds ]
    let typ = Type (TypeCon (TCon "Tuple") typs)
    pure $ Derivation (J ctx exp typ) ds


refineDerivation :: Derivation -> Infer Derivation
refineDerivation d = do
  subst <- ICurrentSubst
  let f ty = refineTypeWithSubst ty subst
  pure $ mapTypeInDerivation f d

data Derivation = Derivation Judgement [Derivation]
data Judgement = J Ctx Exp Type

instance Pretty Derivation where
  pretty d = loop 0 d
    where
      loop :: Int -> Derivation -> String
      loop n (Derivation j ds) = do
        let tab = replicate (2*n) ' '
        concat (map (loop (n+1)) ds) <> "\n" <> tab <> pretty j

instance Pretty Judgement where
  pretty (J ctx exp typ) =
    pretty ctx <> " |= " <> pretty exp <> " :: " <> pretty typ

mapTypeInDerivation :: (Type -> Type) -> Derivation -> Derivation
mapTypeInDerivation f (Derivation j ds) =
  Derivation (mapTypeInJudgement f j) (map (mapTypeInDerivation f) ds)

mapTypeInJudgement :: (Type -> Type) -> Judgement -> Judgement
mapTypeInJudgement f (J ctx exp ty) =
  J (mapTypeInCtx f ctx) exp (f ty)

mapTypeInCtx :: (Type -> Type) -> Ctx -> Ctx
mapTypeInCtx f Ctx{xmap} = Ctx { xmap = Map.map f xmap }

data Ctx = Ctx { xmap :: Map Id Type }
instance Pretty Ctx where pretty Ctx{xmap=m} = pretty m

ctx0 :: Ctx
ctx0 = Ctx { xmap = Map.fromList [ (mkUserId x, ty) | (x,ty) <- init ] }
  where
    init =
      [ ("true", typeBool)
      , ("false", typeBool)
      ]

unify :: Type -> Type -> Infer ()
unify ty1 ty2 = do
  ty1 <- refine ty1
  ty2 <- refine ty2
  --IDebug ("unify: " <> pretty ty1 <> " ~ " <> pretty ty2)
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
refine ty = refineTypeWithSubst ty <$> ICurrentSubst

subTy :: UniVar -> Type -> Infer ()
subTy v ty = if v `occurs` ty then IFail "occurs" else ISub v ty

instance Functor Infer where fmap = liftM
instance Applicative Infer where pure = IPure; (<*>) = ap
instance Monad Infer where (>>=) = IBind

data Infer a where
  IPure :: a -> Infer a
  IBind :: Infer a -> (a -> Infer b) -> Infer b
  IFresh :: String -> Infer UniVar
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
      IDebug mes -> \k -> do
        putStrLn mes
        k s ()
      IFresh _who -> \k -> do
        let IState{u} = s
        let var = UniVar u
        --putStrLn $ "fresh(" <> _who <> "): -> " <> pretty var
        k s { u = u + 1 } var
      ISub v ty -> \k -> do
        --putStrLn ("sub: " <> pretty v <> " := " <> pretty ty)
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

data Subst = Subst { vmap :: Map UniVar Type }
instance Pretty Subst where pretty Subst{vmap=m} = pretty m

subst0 :: Subst
subst0 = Subst { vmap = Map.empty }

subExtend :: Subst -> UniVar -> Type -> Subst
subExtend subst v ty = do
  let Subst{vmap = vmap0} = subst
  let ty' = refineTypeWithSubst ty subst
  let f v' = if v == v' then Just ty' else Nothing
  let shifted = Map.map (specialize f) vmap0
  let vmap = Map.insert v ty' shifted
  Subst {vmap}

typeInt,typeBool :: Type
typeInt = Type (TypeCon (TCon "Int") [])
typeBool = Type (TypeCon (TCon "Bool") [])

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

refineTypeWithSubst :: Type -> Subst -> Type
refineTypeWithSubst ty Subst{vmap} = specialize (\v -> Map.lookup v vmap) ty

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
