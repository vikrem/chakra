{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Chakra.Typescript where

import Protolude
import qualified Data.Text as T

import Data.Aeson.Types (Value)
import Types
import Chakra
import GHC.TypeLits
import GHC.Generics


type family TsType a :: Symbol where
  TsType Int = "number"
  TsType Integer = "number"
  TsType Double = "number"
  TsType Float = "number"
  TsType T.Text = "string"
  TsType Value = "any"
  TsType Bool = "true | false"
  TsType (Maybe a) = AppendSymbol (TsType a) " | null"
  TsType () = "void"
  TsType [a] = (AppendSymbol (TsType a) "[]")
  TsType a = (GTsType (Rep a))

class GHasTsType (r :: * -> *) where
  type GTsType r :: Symbol

instance GHasTsType (M1 S ('MetaSel ('Just fieldName) a b c) (Rec0 typ)) where
  type GTsType (M1 S ('MetaSel ('Just fieldName) a b c) (Rec0 typ)) = AppendSymbol fieldName (AppendSymbol ": " (TsType typ))

instance GHasTsType b => GHasTsType (D1 a b) where
  type GTsType (D1 a b) = GTsType b

instance GHasTsType b => GHasTsType (C1 a b) where
  type GTsType (C1 a b) = AppendSymbol "{ " (AppendSymbol (GTsType b) " }")

instance (GHasTsType a, GHasTsType b) => GHasTsType (a :*: b) where
  type GTsType (a :*: b) = AppendSymbol (GTsType a) (AppendSymbol ", " (GTsType b))

instance (GHasTsType a, GHasTsType b) => GHasTsType (a :+: b) where
  type GTsType (a :+: b) = AppendSymbol (GTsType a) (AppendSymbol " | " (GTsType b))

data TsFunctionDecl = TsFunctionDecl {
  fnName :: T.Text,
  fnTypes :: [T.Text],
  fnVarNames :: [T.Text],
  fnRetType :: T.Text
  } deriving (Show, Eq, Ord, Generic)

data AnyInjectible = forall a. (HasTSDecl (JsType a), JsTypeable a) => AnyInjectible a

instance Monoid TsFunctionDecl where
  mempty = TsFunctionDecl "" [] [] ""
  mappend (TsFunctionDecl a b c d) (TsFunctionDecl a' b' c' d') =
    TsFunctionDecl (a <> a') (b <> b') (c <> c') (d <> d')

class HasTSDecl a where
  tsDecl :: Proxy a -> TsFunctionDecl

instance (KnownSymbol (TsType a)) => HasTSDecl (JsFnTypelist '[] a) where
  tsDecl _ = mempty {fnRetType = typeName}
    where
      typeName = T.pack $ symbolVal $ Proxy @(TsType a)

instance {-# OVERLAPS #-} (KnownSymbol (TsType a)) => HasTSDecl (JsFnTypelist '[] (JsPromise a)) where
  tsDecl _ = mempty {fnRetType = "Promise<" <> typeName <> ">" }
    where
      typeName = T.pack $ symbolVal $ Proxy @(TsType a)

instance (KnownSymbol (TsType a),
          HasTSDecl (JsFnTypelist xs b)
         ) => HasTSDecl (JsFnTypelist (a ': xs) b) where
  tsDecl _ = mempty {fnTypes = [typeName]} <> tsDecl (Proxy @(JsFnTypelist xs b))
    where
      typeName = T.pack $ symbolVal $ Proxy @(TsType a)

getDecl :: forall a b. (JsType a ~ b, HasTSDecl b) => a -> TsFunctionDecl
getDecl _ = tsDecl (Proxy @b)

printDecl :: TsFunctionDecl -> T.Text
printDecl TsFunctionDecl{..} =
  "function " <> fnName <> "(" <> argList <> "): " <> fnRetType <> ";"
  where
    argList :: T.Text
    argList = T.intercalate ", " $
      zipWith (\n t -> n <> ": " <> t) (fnVarNames <> (T.singleton <$> ['a'..'z'])) fnTypes

getNamespaceDecl :: T.Text -> [TsFunctionDecl] -> T.Text
getNamespaceDecl n fns = T.unlines [
    "declare namespace " <> n <> " {"
  , "  " <> T.unlines (printDecl <$> fns)
  , "}"
  ]

injectNamespace :: T.Text -> [(AnyInjectible, T.Text)] -> (Chakra (), T.Text)
injectNamespace name fns = (injectAction, decl)
  where
    decl = getNamespaceDecl name $
      (\(AnyInjectible a, funcName) -> mempty {fnName = funcName} <> getDecl a) <$> fns
    injectAction = sequence_ $ (\(AnyInjectible a, funcName) -> injectChakra a [name] funcName) <$> fns
