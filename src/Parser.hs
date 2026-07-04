module Parser (parse) where

import AST (Bid(..),Exp,Id,mkUserId)
import AST qualified
import Data.Char qualified as Char (isAlpha,isNumber,isLower)
import Data.Text qualified as Text (pack)
import Par4 (Par,noError,skip,alts,many,some,sat,position,Pos(..))
import Par4 qualified (parse)
import Text.Printf (printf)

parse :: String -> Exp -- Prog
parse s =
  case Par4.parse gram6 (Text.pack s) of
    Right x -> x
    Left msg -> error msg

mkAbstraction :: [Bid] -> Exp -> Exp
mkAbstraction xs e = case xs of [] -> e; x@(Bid pos _):xs -> AST.Lam pos x (mkAbstraction xs e)

mkApps :: Exp -> [(Pos,Exp)] -> Exp
mkApps f es = case es of [] -> f; (pos,e):es -> mkApps (AST.App f pos e) es

{-mkIte :: Pos -> Exp -> Pos -> Exp -> Pos -> Exp -> Exp
mkIte pos i posThen t posElse e =
  AST.Match pos i [AST.Arm posThen cTrue [] t, AST.Arm posElse cFalse [] e ]-}

underscore :: Id
underscore = mkUserId "_"

{-positioned :: Par a -> Par (Pos,a)
positioned par = do
  pos <- position
  x <- par
  pure (pos,x)-}

data Precedence = L | R

gram6 :: Par Exp --Prog
gram6 = topExp where
--gram6 = program where

  keywords = [] --"let","in","if","then","else","fun","match","with","rec","true","false","type","of","assert"]

  fail = alts []

  lit x = do _ <- sat (== x); pure ()
  white1 = alts [lit ' ', lit '\n', lit '\t']

  next = sat (\_ -> True)

  comment = do
    noError $ do lit '('; lit '*'
    nest 0
    where
      nest :: Int -> Par ()
      nest i =
        next >>= \case
        '*' -> do next >>= \case ')' -> if i == 0 then pure () else nest (i-1); _ -> nest i
        '(' -> do next >>= \case '*' -> nest (i+1); _ -> nest i
        _ -> nest i

  whitespace = skip (alts [white1, comment])

  decDigit = alts [ do lit c; pure n | (c,n) <- zip "0123456789" [0..] ]

  nibble par = do
    x <- par
    whitespace
    pure x

  -- nibbling from here...

  isVariableChar1 c = Char.isLower c || c == '_'
  -- isConstructorChar1 c = Char.isUpper c
  isIdentifierChar c = Char.isNumber c || Char.isAlpha c || c `elem` "'_"

{-  tvar = nibble $ do
    lit '\''
    _xs <- some $ sat isIdentifierChar
    pure ()-}

  key s =
    if all isIdentifierChar s && s `notElem` keywords
    then error (printf "Add \"%s\" to keywords list" s)
    else nibble (noError (mapM_ lit s))

  bracketedOperatorName = nibble $ noError $ do
    lit '('
    s <- alts [ noError $ do mapM_ lit name; pure name
              | name <- infixNames ++ prefixNames ]
    lit ')'
    pure (mkUserId s)

  identifier = mkUserId <$> noError name
    where
      name = do
        x <- sat isVariableChar1
        xs <- many $ sat isIdentifierChar
        let s = x:xs
        if s `elem` keywords then fail else nibble (pure s)

{-  constructor0 = AST.Cid <$> do
    x <- sat isConstructorChar1
    xs <- many $ sat isIdentifierChar
    let s = x:xs
    nibble (pure s)

  constructor = alts
    [ constructor0
    , do key "true"; pure cTrue
    , do key "false"; pure cFalse
    ]-}

  decNumber :: Par Int = foldl (\acc d -> 10*acc + d) 0 <$> some decDigit

  number = nibble $ alts [decNumber]

  charLitPlain = sat $ \c -> c /= '\\'

  charLitEscaped = do
    lit '\\'
    alts
      [ do lit '\\'; pure '\\'
      , do lit 'n'; pure '\n'
      , do lit 'b'; pure '\b'
      , do lit 't'; pure '\t'
      , do lit '"'; pure '"'
      ]

  singleQuote = lit '\''
  charLit = nibble $ do
    singleQuote
    x <- alts [charLitEscaped, charLitPlain]
    singleQuote
    pure x

  stringLitChar = sat $ \c -> c /= '"' && c /= '\\'
  doubleQuote = lit '"'
  stringLit = nibble $ do
    doubleQuote
    x <- many (alts [charLitEscaped, stringLitChar])
    doubleQuote
    pure x

  openClose = noError $ do
    key "("
    key ")"

  bracketed thing = do
    key "("
    x <- thing
    key ")"
    pure x

  identOrUnit :: Par Id =
    alts [identifier
         , do openClose; pure underscore
         ]

  -- patterns...
{-
  nilPat = do
    key "["
    key "]"
    pure (cNil,[])

  consPat = do
    x <- bound identOrUnit
    key "::"
    xs <- bound identOrUnit
    pure (cCons,[x,xs])
-}
{-  tupleId :: Par [Bid] =
    alts [ bracketed (separated (key ",") (bound identOrUnit))
         , do x <- bound identOrUnit; pure [x]
         , pure []
         ]-}
{-
  constructedPat = do
    c <- constructor
    xs <- tupleId
    pure (c,xs)

  pat :: Par (Cid, [Bid]) =
    alts [nilPat,consPat,constructedPat]
-}
  -- expressions...

  var = do
    pos <- position
    x <- alts [identifier,bracketedOperatorName]
    pure (AST.Var pos x)

  positionedLit = do
    pos <- position
    lit <- alts
      [ AST.LitN <$> number
      , AST.LitC <$> charLit
      , AST.LitS <$> stringLit
      ]
    pure $ AST.Lit pos lit

{-
  unit = do
    pos <- position
    openClose
    pure (AST.Con pos cUnit [])

  ignored_assert = do
    pos <- position
    key "assert"
    _ <- atom
    pure (AST.Con pos cUnit [])
-}
  literal = alts [positionedLit] --,unit]

{-  tupleExp :: Par [Exp] =
    bracketed (separated (key ",") exp)-}

{-
  listExp = do
    openPos <- position
    key "["
    alts [ do key "]"; pure $ AST.Con openPos cNil []
         , do
             elems <- separated (key ";") (positioned expITE)
             closePos <- position
             key "]"
             let
               mkList :: [(Pos,Exp)] -> Exp
               mkList = \case
                 [] -> AST.Con closePos cNil []
                 (pos,e1):es -> AST.Con pos cCons [e1,mkList es]
             pure (mkList elems)
         ]

  consApp = do
    pos <- position
    c <- constructor
    alts
      [ do es <- tupleExp; pure (AST.Con pos c es)
      , do e <- atom; pure (AST.Con pos c [e])
      , pure (AST.Con pos c [])
      ]

  cons0 = do
    pos <- position
    c <- constructor
    pure (AST.Con pos c [])
-}
  atom0 = alts [literal,var
               -- ,listExp
               ,bracketed exp
               --,cons0
               ]

  prefixNames = ["!"]

  prefixed = do
    p1 <- position
    name <- alts [ do key x; pure x | x <- prefixNames ]
    p <- position
    arg <- atom0
    pure (AST.App (AST.Var p1 (mkUserId name)) p arg)

  atom = alts [ atom0, prefixed ]

  application = do
    let loop f = alts [ pure f , do p <- position; e <- atom; loop (AST.App f p e)]
    atom >>= loop

  mkBinApp :: Pos -> Pos -> String -> Exp -> Exp -> Exp
  mkBinApp p1 p2 name x1 x2 =
    case name of
--      "||" -> mkIte p1 x1 p1 (eTrue p1) p2 x2
--      "&&" -> mkIte p1 x1 p2 x2 p2 (eFalse p2)

      _ -> mkApps (AST.Var p1 (mkUserId name)) [(p1,x1),(p2,x2)]

--  eTrue pos = AST.Con pos cTrue []
--  eFalse pos = AST.Con pos cFalse []

  infixOpL names sub = sub >>= loop where
    loop acc =
      alts [ pure acc
           , do
               p1 <- position
               name <- alts [ do key x; pure x | x <- names ]
               p2 <- position
               x <- sub
               loop (mkBinApp p1 p2 name acc x)
           ]

  infixOpR names sub = do
    x <- sub
    alts [ pure x
         , do
             p1 <- position
             name <- alts [ do key x; pure x | x <- names ]
             p2 <- position
             y <- infixOpR names sub
             pure (mkBinApp p1 p2 name x y)
         ]

  infixOp :: (Precedence,[String]) -> Par Exp -> Par Exp
  infixOp (p,xs) = case p of L -> infixOpL xs; R -> infixOpR xs

  -- higest..lowest
  infixGroup1 = (L,["*","%","/"])
  infixGroup2 = (L,["+","-"])
  infixGroup3 = (R,["::"])
  infixGroup4 = (R,["^","@@","@"])
  infixGroup5 = (L,["=","<=","<",">=",">"])
  infixGroup6 = (R,["&&"])
  infixGroup7 = (R,["||"])
  infixGroup8 = (R,[":="])

  infixNames = concat (map snd [infixGroup1,infixGroup2,infixGroup3,infixGroup4
                               ,infixGroup5,infixGroup6,infixGroup7,infixGroup8])

  infix0 = alts [--consApp,
                application] --, ignored_assert]
  infix1 = infixOp infixGroup1 infix0
  infix2 = infixOp infixGroup2 infix1
  infix3 = infixOp infixGroup3 infix2
  infix4 = infixOp infixGroup4 infix3
  infix5 = infixOp infixGroup5 infix4
  infix6 = infixOp infixGroup6 infix5
  infix7 = infixOp infixGroup7 infix6
  infix8 = infixOp infixGroup8 infix7

  infixWeakestPrecendence = infix8

  bound :: Par Id -> Par Bid
  bound identPar = do
    pos <- position
    x <- identPar
    pure (Bid pos x)

  bindingAbstraction = do
    xs <- many (bound identOrUnit)
    key "="
    e <- exp
    pure (mkAbstraction xs e)

  binding :: Par (Bid,Exp) = do
    key "let"
    pos <- position
    r <- alts [ do key "[@unroll]"; key "rec"; pure (Just True)
              , do key "rec"; pure (Just False)
              , pure Nothing]
    f <- bound $ alts [identOrUnit,bracketedOperatorName]
--    _ <- opt type_annotation
    case r of
      Just unroll -> do
        bindingAbstraction >>= \case
          AST.Lam _ x1 rhs -> pure (f, AST.RecLam pos unroll f x1 rhs)
          _ -> fail
      Nothing -> do
        rhs <- bindingAbstraction
        pure (f,rhs)

  let_ = do
    pos <- position
    (x,rhs) <- binding
    key "in"
    body <- exp
    pure (AST.Let pos x rhs body)

{-  ite = do
    pos <- position
    key "if"
    i <- exp
    posThen <- position
    key "then"
    t <- exp
    posElse <- position
    key "else"
    e <- exp_no_semi
    pure (mkIte pos i posThen t posElse e)-}

  abstraction = do
    key "\\" --fun
    xs <- some (bound identOrUnit)
    key "->"
    e <- exp
    pure (mkAbstraction xs e)
{-
  arm :: Par Arm = do
    (c,xs) <- pat
    pos <- position
    key "->"
    e <- exp
    pure (AST.Arm pos c xs e)

  match_ = do
    pos <- position
    key "match"
    e <- exp
    key "with"
    _ <- opt (key "|")
    as <- separated (key "|") arm
    pure (AST.Match pos e as)
-}
  expITE = alts
    [ infixWeakestPrecendence
    -- , ite
    ]

  expSEQ = do
    e1 <- expITE
    alts [ do pos <- position
              key ";"
              e2 <- exp
              pure (AST.Let pos (Bid pos underscore) e1 e2)
         , pure e1
         ]

{-  exp_no_semi = alts
    [ expITE
    , abstraction
    -- , match_
    , let_
    ]-}

  exp = alts
    [ expSEQ
    , abstraction
    -- , match_
    , let_
    ]

{-
  value_def = do
    (x,rhs) <- binding
    pure (AST.ValDef x rhs)

  -- types and typedefs: mostly skipped

  type_constructor :: Par () = do
    alts
      [ do _ <- identifier; pure ()
      ]
    pure ()

  atomic_type = do
    _ <- opt (alts [ do _ <- tvar; pure ()
                   , do _ <- bracketed (separated (key ",") type_); pure ()
                   ])
    _ <- many type_constructor
    pure ()

  type_ = separated (alts [key "*", key "->"]) atomic_type

  type_annotation = do
    key ":"
    type_
-}
{-  maybe_tvar_seq =
    alts [ tvar
         , do _ <- bracketed (separated (key ",") tvar); pure ()
         , pure ()
         ]-}

{-  of_type = do
    key "of"
    type_-}

{-
  type_def_arm = do
    cid <- constructor
    _ <- opt of_type
    pure cid

  type_def_or_skip_alias = do
    key "type"
    maybe_tvar_seq
    type_constructor
    key "="
    alts [ do
             cids <- separated (key "|") type_def_arm
             pure (Just (AST.TypeDef cids))
         , do
             _ <- type_
             pure Nothing
         ]
-}
{-  many_defs = loop []
    where
      loop acc =
        alts
        [ pure (reverse acc)
        , do d <- value_def; loop (d:acc)
{-        , type_def_or_skip_alias >>= \case
            Just d -> loop (d:acc)
            Nothing -> loop acc-}
        ]

  _program = do
    whitespace
    ds <- many_defs
    pure $ AST.Prog ds
-}

  topExp = do
    whitespace
    exp

