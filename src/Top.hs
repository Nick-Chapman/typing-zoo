module Top (main) where

import AST (Exp,Id,mkUserId)
import AST qualified (Exp(..),Bid(..),Literal(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Parser (parse)
import Pretty (Pretty(..))
import Infer (IType,TypeError,Infer(..),typeBase0,tuple,unify,(-->),getRefine2,runInfer,ITypeScheme,generalize,instantiate,mono)
import TypeF (TypeScheme)
import System.Environment (getArgs)

main :: IO ()
main = do
  --putStrLn "*typing-zoo*"
  args <- getArgs
  let Config {filename} = parseConfig args
  xs <- zip [0..] . filterExamples . lines <$> readFile filename
  mapM_ runExample xs
    where
      _pick ns xs = [ (n, xs!!n) | n <- ns ]
      filterExamples = filter (not . empty) . map dropComment
      dropComment :: String -> String
      dropComment = takeWhile (/= '#')
      empty :: String -> Bool
      empty s = s==""

data Config = Config { filename :: String }
parseConfig :: [String] -> Config
parseConfig = \case
  [filename] -> Config {filename}
  args -> error ("parseConfig: " <> show args)


runExample :: (Int,String) -> IO ()
runExample (i,s) = do
  let trim = reverse . dropWhile (==' ') . reverse
  putStrLn $ "[" <> show i <> "] " <> trim s
  let exp = parse s
  --putStrLn $ "[" <> show i <> "] " <> pretty exp
  runInferTypeOfExp exp >>= \case
    Left err -> putStrLn ("**type error: " <> pretty err)
    Right ty -> do
      putStrLn (":: " <> pretty ty)

runInferTypeOfExp :: Exp -> IO (Either TypeError TypeScheme)
runInferTypeOfExp exp = do
  runInfer $ do
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
    let err = error ("typeExp/EVar" <> pretty x)
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
      ]

typeInt,typeChar,typeString,typeBool :: IType
typeInt = typeBase0 "Int"
typeChar = typeBase0 "Char"
typeString = typeBase0 "String"
typeBool = typeBase0 "Bool"
