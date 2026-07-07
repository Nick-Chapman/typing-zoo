module Top (main) where

import AST (Exp,Id,mkUserId)
import AST qualified (Exp(..),Bid(..),Literal(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Parser (parse)
import Pretty (Pretty(..))
import Infer (IType,TypeError,Infer(..),typeInt,typeBool,tuple,unify,(-->),getRefine1,getRefine2,runInfer)
import TypeF (FixType)

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

runInferTypeOfExp :: Exp -> IO (Either TypeError FixType)
runInferTypeOfExp exp = do
  runInfer $ do
    t <- typeExp ctx0 exp
    _refine <- getRefine1
    refine <- getRefine2
    pure (refine t)

typeExp :: Ctx -> Exp -> Infer IType
typeExp ctx exp = case exp of
  AST.Lam _pos (AST.Bid _ x) body -> do
    let Ctx{xmap} = ctx
    arg <- IFresh
    IDebug $ "fresh(" <> pretty x <> "): -> " <> pretty arg
    let ctx1 = ctx { xmap = Map.insert x arg xmap }
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
    ts <- mapM (typeExp ctx) es
    pure $ tuple ts

data Ctx = Ctx { xmap :: Map Id IType }
instance Pretty Ctx where pretty Ctx{xmap=m} = pretty m

ctx0 :: Ctx
ctx0 = Ctx { xmap = Map.fromList [ (mkUserId x, ty) | (x,ty) <- init ] }
  where
    init =
      [ ("true", typeBool)
      , ("false", typeBool)
      ]
