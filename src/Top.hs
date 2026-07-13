module Top (main) where

import Alg (typeOfExp)
import Infer (runInfer)
import Parser (parse)
import Pretty (Pretty(..))
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let Config {filename} = parseConfig args
  xs <- zip [0..] . filterExamples . lines <$> readFile filename
  mapM_ runExample xs
    where
      _pick ns xs = [ (n, xs!!n) | n <- ns ]
      filterExamples = filter (not . isEmpty) . map dropComment
      dropComment = takeWhile (/= '#')
      isEmpty s = s==""

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
  runInfer (typeOfExp exp) >>= \case
    Left err -> putStrLn ("**type error: " <> pretty err)
    Right ty -> do
      putStrLn (":: " <> pretty ty)
