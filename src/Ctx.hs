module Ctx
  ( Ctx
  , ctx0
  , typeBool,typeChar,typeString,typeInt
  , lookupCtx, insertCtx
  , typesFromCtx
  )
  where

import AST (Id,mkUserId)
import Data.Map (Map)
import Data.Map qualified as Map
import Infer (IType,ITypeScheme,typeBase0,(-->),mono)
import Pretty (Pretty(..))

data Ctx = Ctx { xmap :: Map Id ITypeScheme }
instance Pretty Ctx where pretty Ctx{xmap=m} = pretty m

lookupCtx :: Ctx -> Id -> ITypeScheme
lookupCtx Ctx{xmap} x =
  maybe err id $ Map.lookup x xmap
  where err = error ("unbound var: '" <> pretty x <> "'")

insertCtx :: Ctx -> Id -> ITypeScheme -> Ctx
insertCtx ctx@Ctx{xmap} x v = ctx { xmap = Map.insert x v xmap }

typesFromCtx :: Ctx -> [ITypeScheme]
typesFromCtx Ctx{xmap} = [ s | (_,s) <- Map.toList xmap ]

ctx0 :: Ctx
ctx0 = Ctx { xmap = Map.fromList [ (mkUserId x, mono ty) | (x,ty) <- init ] }
  where
    init =
      [ ("true", typeBool)
      , ("false", typeBool)
      , ("-", typeInt --> typeInt --> typeInt)
      , ("+", typeInt --> typeInt --> typeInt)
      , ("*", typeInt --> typeInt --> typeInt)
      , ("/", typeInt --> typeInt --> typeInt)
      , ("&&", typeBool --> typeBool --> typeBool)
      , ("||", typeBool --> typeBool --> typeBool)
      , ("not", typeBool --> typeBool)
      ]

typeInt,typeChar,typeString,typeBool :: IType
typeInt = typeBase0 "Int"
typeChar = typeBase0 "Char"
typeString = typeBase0 "String"
typeBool = typeBase0 "Bool"
