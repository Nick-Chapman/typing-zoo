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
    Right ty -> do
      putStrLn (":: " <> pretty ty)

runInferTypeOfExp :: Exp -> IO (Either TypeError Type)
runInferTypeOfExp exp = do
  runInfer $ do
    t <- typeExp ctx0 exp
    refine <- getRefine
    pure (refine t)

typeExp :: Ctx -> Exp -> Infer Type
typeExp ctx exp = case exp of
  AST.Lam _pos (AST.Bid _ x) body -> do
    let Ctx{xmap} = ctx
    typArg <- TypeUnknown <$> IFresh
    IDebug $ "fresh(" <> pretty x <> "): -> " <> pretty typArg
    let ctx1 = ctx { xmap = Map.insert x typArg xmap }
    typRes <- typeExp ctx1 body
    pure $ Type (typArg :-> typRes)
  AST.App fun _pos arg -> do
    typRes <- TypeUnknown <$> IFresh
    IDebug $ "fresh(" <> pretty exp <> "): -> " <> pretty typRes
    typFun <- typeExp ctx fun
    typArg <- typeExp ctx arg
    unify typFun (Type (typArg :-> typRes))
    pure typRes
  AST.Var _pos x -> do
    let Ctx{xmap} = ctx
    let err = error ("typeExp/EVar" <> pretty x)
    pure $ maybe err id $ Map.lookup x xmap
  AST.Lit _pos  lit ->
    case lit of
      AST.LitN{} -> pure typeInt
      AST.LitC{} -> undefined
      AST.LitS{} -> undefined
  AST.RecLam{} -> undefined
  AST.Let pos x rhs body -> do -- temp; prior to support generalization
    let func = AST.Lam pos x body
    let appliedAbstraction = AST.App func pos rhs
    typeExp ctx appliedAbstraction
  AST.Tuple es -> do
    typs <- mapM (typeExp ctx) es
    pure $ Type (TypeCon (TCon "Tuple") typs)

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
