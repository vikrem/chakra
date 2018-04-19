{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Chakra.Typescript where

import Protolude
import qualified Data.Text as T

import Data.Aeson.Types (Value)
import Types
import Chakra

import Data.String (String)

type family TsType a :: Symbol where
  TsType Int = "number"
  TsType String = "string"
  TsType T.Text = "string"
  TsType Value = "any"

data TsFunctionDecl = TsFunctionDecl {
  fnName :: T.Text,
  fnTypes :: [T.Text],
  fnVarNames :: [T.Text],
  fnRetType :: T.Text
  } deriving (Show, Eq, Ord)

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
