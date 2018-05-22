{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Chakra.Typescript where

import Protolude
import qualified Data.Text as T

import Data.Aeson.Types (Value)
import Types
import Chakra
import GHC.TypeLits
import GHC.Generics
import Data.Time.Clock (UTCTime)
import Data.UUID.Types (UUID)


symtext :: forall a. KnownSymbol a => Proxy a -> T.Text
symtext _ = T.pack $ symbolVal (Proxy @a)

type family TsType a :: Symbol where
  TsType Int = "number"
  TsType Integer = "number"
  TsType Double = "number"
  TsType Float = "number"
  TsType T.Text = "string"
  TsType Value = "any"
  TsType Bool = "true | false"
  TsType UUID = "string"
  TsType UTCTime = "string"
  TsType (Maybe a) = AppendSymbol (TsType a) " | null"
  TsType () = "void"
  TsType (JsPromise a) = AppendSymbol "Promise<" (AppendSymbol (TsType a) ">")
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

data Injectible = Namespace T.Text [Injectible] |
  forall a. (HasTSDecl (JsType a), JsTypeable a) => HsFunc T.Text a

instance Monoid TsFunctionDecl where
  mempty = TsFunctionDecl "" [] [] ""
  mappend (TsFunctionDecl a b c d) (TsFunctionDecl a' b' c' d') =
    TsFunctionDecl (a <> a') (b <> b') (c <> c') (d <> d')

class HasTSDecl a where
  tsDecl :: Proxy a -> TsFunctionDecl

data TsSpec = TsFunctionD {
  fnName' :: T.Text,
  fnTypes' :: [T.Text],
  fnVarNames' :: [T.Text],
  fnRetType' :: T.Text
  } |
  TsNamespace {
    nsName :: T.Text,
    nsContents :: [TsSpec]
            }
  deriving (Show, Eq, Ord, Generic)
-- API building

data NS (n :: Symbol) (a :: [*])
data Fn (name :: Symbol)
data Arg (name :: Symbol) (typ :: *)
data Ret (typ :: *)
data f1 :> f2
infixr 8 :>
data f1 :+ f2
infixr 9 :+

indent :: T.Text -> T.Text
indent s = T.unlines $ ("  " <> ) <$> T.lines s

class HasTSSpec (a :: b) where
  tsSpec :: Proxy a -> T.Text

class HasTSSpecList (a :: [*]) where
  tsSpecList :: Proxy a -> [T.Text]

instance HasTSSpecList '[] where
  tsSpecList _ = []

instance (HasTSSpec x, HasTSSpecList xs) => HasTSSpecList (x ': xs) where
  tsSpecList _ = (tsSpec (Proxy @x)) : (tsSpecList (Proxy @xs))

instance (HasTSSpecList xs, KnownSymbol n) => HasTSSpec (NS n xs) where
  tsSpec _ = "declare namespace " <> (T.pack $ symbolVal $ Proxy @n) <> " {\n" <>
    T.concat (indent <$> (tsSpecList $ Proxy @xs)) <>
    "}"

instance (KnownSymbol n, HasTSSpec xs) => HasTSSpec ((Fn n) :> xs) where
  tsSpec _ = "function " <> T.pack (symbolVal $ Proxy @n) <> "(" <> tsSpec (Proxy @xs)

instance {-# OVERLAPS #-} (KnownSymbol (TsType t),
          HasTSSpec (Arg n' t' :> xs),
          KnownSymbol n
         ) => HasTSSpec ((Arg n t) :> (Arg n' t' :> xs)) where
  tsSpec _ = (symtext $ Proxy @n) <> ": " <>
    symtext (Proxy @(TsType t)) <> ", " <>
    tsSpec (Proxy @(Arg n' t' :> xs))

instance (HasTSSpec xs, KnownSymbol n, KnownSymbol (TsType t)) => HasTSSpec (Arg n t :> xs) where
  tsSpec _ = (symtext $ Proxy @n) <> ": " <>
    symtext (Proxy @(TsType t)) <> ")"

instance KnownSymbol (TsType t) => HasTSSpec (Ret t) where
  tsSpec _ = ": " <> symtext (Proxy @(TsType t))

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

indentBlock :: T.Text -> T.Text
indentBlock = T.unlines . map ("  " <>)  . T.lines

printInjectible :: Injectible -> T.Text
-- TLD declare
printInjectible (Namespace nsName inj) = "declare " <> printInjectible' (Namespace nsName inj)
printInjectible x = printInjectible' x

printInjectible' :: Injectible -> T.Text
printInjectible' (HsFunc name f) = printDecl $ mempty { fnName = name } <> getDecl f
printInjectible' (Namespace nsName inj) = T.unlines [
    "namespace " <> nsName <> " {"
  , T.unlines $ indentBlock . printInjectible' <$> inj
  , "}"
  ]

injectInjectible :: Injectible -> Chakra ()
injectInjectible = injectInjectible' []

injectInjectible' :: [T.Text] -> Injectible -> Chakra ()
injectInjectible' ns (HsFunc name f) = injectChakra f ns name
injectInjectible' ns (Namespace name injs) = sequence_ $ injectInjectible' (ns ++ [name]) <$> injs

getDecl :: forall a b. (JsType a ~ b, HasTSDecl b) => a -> TsFunctionDecl
getDecl _ = tsDecl (Proxy @b)

printDecl :: TsFunctionDecl -> T.Text
printDecl TsFunctionDecl{..} =
  "function " <> fnName <> "(" <> argList <> "): " <> fnRetType <> ";"
  where
    argList :: T.Text
    argList = T.intercalate ", " $
      zipWith (\n t -> n <> ": " <> t) (fnVarNames <> (T.singleton <$> ['a'..'z'])) fnTypes
