module Top (main) where

import AST (Exp,Id,mkUserId)
import AST qualified (Exp(..),Bid(..),Literal(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Parser (parse)
import Pretty (Pretty(..))
import TypeF (TypeF(..),TCon(..))
import Infer (Type(..),TypeError,Infer(..),getRefine,unify,runInfer)

main :: IO ()
main = do
  putStrLn "*typing-zoo*"
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
        Right (_d@(Derivation (J _ _ _ty) _)) -> do
          putStrLn (":: " <> pretty _ty)
          --putStrLn ("derivation: " <> pretty _d)

runInferTypeOfExp :: Exp -> IO (Either TypeError Derivation)
runInferTypeOfExp exp = do
  runInfer $ do
    d <- typeExp ctx0 exp
    refineDerivation d

refineDerivation :: Derivation -> Infer Derivation
refineDerivation d = do
  refine <- getRefine
  pure $ mapTypeInDerivation refine d

typeExp :: Ctx -> Exp -> Infer Derivation
typeExp ctx exp = case exp of
  AST.Lam _pos (AST.Bid _ x) body -> do
    let Ctx{xmap} = ctx
    typArg <- TypeUnknown <$> IFresh
    IDebug $ "fresh(" <> pretty x <> "): -> " <> pretty typArg
    let ctx1 = ctx { xmap = Map.insert x typArg xmap }
    d1@(Derivation (J _ _ typRes) _) <- typeExp ctx1 body
    let typFun = Type (typArg :-> typRes)
    pure $ Derivation (J ctx exp typFun) [d1]
  AST.App fun _pos arg -> do
    typRes <- TypeUnknown <$> IFresh
    IDebug $ "fresh(" <> pretty exp <> "): -> " <> pretty typRes
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

data Ctx = Ctx { xmap :: Map Id Type }
instance Pretty Ctx where pretty Ctx{xmap=m} = pretty m

ctx0 :: Ctx
ctx0 = Ctx { xmap = Map.fromList [ (mkUserId x, ty) | (x,ty) <- init ] }
  where
    init =
      [ ("true", typeBool)
      , ("false", typeBool)
      ]

typeInt,typeBool :: Type
typeInt = Type (TypeCon (TCon "Int") [])
typeBool = Type (TypeCon (TCon "Bool") [])

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
