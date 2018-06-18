{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Chakra.Typescript where

import Protolude hiding (TypeError, (:+))
import qualified Data.Text as T

import Data.Aeson.Types (Value, Object)
import Data.List (init, last)
import Types
import Chakra
import GHC.TypeLits
import GHC.Generics
import Data.Time.Clock (UTCTime)
import Data.UUID.Types (UUID)

symtext :: forall a. KnownSymbol a => Proxy a -> T.Text
symtext _ = T.pack $ symbolVal (Proxy @a)

-- map hs types to ts type strings
type family TsType a :: Symbol where
  TsType Int = "number"
  TsType Integer = "number"
  TsType Double = "number"
  TsType Float = "number"
  TsType T.Text = "string"
  TsType Value = "any"
  TsType Object = "Object"
  TsType Bool = "true | false"
  TsType UUID = "string"
  TsType UTCTime = "string"
  TsType (Maybe a) = AppendSymbol (TsType a) " | null"
  TsType () = "void"
  TsType (HsAsyncFn s a) = AppendSymbol "Promise<" (AppendSymbol (TsType a) ">")
  TsType [a] = (AppendSymbol (TsType a) "[]")
  TsType a = (GTsType (Rep a))

-- The final return type of a native function that returns an `a` to js
type family RetTsType a :: * where
  RetTsType (HsAsyncFn s a) = HsAsyncFn s a
  RetTsType (HsFn s a) = HsFn s a

-- Mechanically generate inline typescript decls for simple datatypes
class GHasTsType (r :: * -> *) where
  type GTsType r :: Symbol

-- Grab field names
instance GHasTsType (M1 S ('MetaSel ('Just fieldName) a b c) (Rec0 typ)) where
  type GTsType (M1 S ('MetaSel ('Just fieldName) a b c) (Rec0 typ)) = AppendSymbol fieldName (AppendSymbol ": " (TsType typ))

instance GHasTsType b => GHasTsType (D1 a b) where
  type GTsType (D1 a b) = GTsType b

instance GHasTsType b => GHasTsType (C1 a b) where
  type GTsType (C1 a b) = AppendSymbol "{ " (AppendSymbol (GTsType b) " }")

-- Product {foo: fooType, bar: barType}
instance (GHasTsType a, GHasTsType b) => GHasTsType (a :*: b) where
  type GTsType (a :*: b) = AppendSymbol (GTsType a) (AppendSymbol ", " (GTsType b))

-- Sum {foo: fooType} | {bar: barType}
instance (GHasTsType a, GHasTsType b) => GHasTsType (a :+: b) where
  type GTsType (a :+: b) = AppendSymbol (GTsType a) (AppendSymbol " | " (GTsType b))

-- API building

data NS (n :: Symbol) (a :: [*]) -- Namespace
data Fn (name :: Symbol) -- Func
data Arg (name :: Symbol) (typ :: *) -- Function argument
data Ret (typ :: *) -- Function return type
data f1 :> f2 -- Assemble function type
infixr 8 :>
data f1 :+ f2 = f1 :+ f2 -- Add functions or namespaces together
infixr 9 :+

indent :: T.Text -> T.Text
indent s = T.unlines $ ("  " <> ) <$> T.lines s

-- "declare namespace" occurs only at top level. "namespace" underneath
data Nesting = TopLevel | Nested

-- Generate .ts.d from an API description
class HasTSSpec (a :: b) where
  tsSpec :: Proxy a -> Nesting -> T.Text

class AllHasTSSpec (a :: [*]) where
  tsSpecList :: Proxy a -> Nesting -> [T.Text]

instance AllHasTSSpec '[] where
  tsSpecList _ _ = []

instance (HasTSSpec x, HasTSSpec y) => HasTSSpec (x :+ y) where
  tsSpec _ nest = tsSpec (Proxy @x) nest <> tsSpec (Proxy @y) nest

instance (HasTSSpec x, AllHasTSSpec xs) => AllHasTSSpec (x ': xs) where
  tsSpecList _ nest = (tsSpec (Proxy @x) nest) : (tsSpecList (Proxy @xs) nest)

instance (AllHasTSSpec xs, KnownSymbol n) => HasTSSpec (NS n xs) where
  tsSpec _ nest = (topDecl nest) <> (T.pack $ symbolVal $ Proxy @n) <> " {\n" <>
    T.concat (indent <$> (tsSpecList (Proxy @xs) Nested)) <>
    "}\n"
    where
      topDecl TopLevel = "declare namespace "
      topDecl Nested = "namespace "

instance (KnownSymbol n, HasTSSpec xs) => HasTSSpec ((Fn n) :> xs) where
  tsSpec _ nest = "function " <> T.pack (symbolVal $ Proxy @n) <> "(" <> tsSpec (Proxy @xs) nest


-- Seperate successive `Arg`s with commas.
-- Fn "foo" :> Arg "bar" BarType :> Arg "baz" BazType :> Ret FooType
instance {-# OVERLAPS #-} (KnownSymbol (TsType t),
          HasTSSpec (Arg n' t' :> xs),
          KnownSymbol n
         ) => HasTSSpec ((Arg n t) :> (Arg n' t' :> xs)) where
  tsSpec _ nest = (symtext $ Proxy @n) <> ": " <>
    symtext (Proxy @(TsType t)) <> ", " <>
    tsSpec (Proxy @(Arg n' t' :> xs)) nest

-- Last arg in a list of Args
instance (HasTSSpec xs, KnownSymbol n, KnownSymbol (TsType t)) => HasTSSpec (Arg n t :> xs) where
  tsSpec _ nest = (symtext $ Proxy @n) <> ": " <>
    symtext (Proxy @(TsType t)) <>
    tsSpec (Proxy @xs) nest

-- Return type. Closes arg list
instance KnownSymbol (TsType t) => HasTSSpec (Ret t) where
  tsSpec _ _ = "): " <> symtext (Proxy @(TsType t))

class HasTSImpl a where
  type TSImpl a :: *
-- Proxy needed because TSImpl isn't injective
  injectAPI :: Proxy a -> [T.Text] -> TSImpl a -> Chakra s ()

class AllHasTSImpl (a :: [*]) where
  injectAllAPI :: Proxy a -> [T.Text] -> TSImplList a -> Chakra s ()

instance (TypeError ('Text "Can't solve for the implementation of an empty namespace")) => AllHasTSImpl '[] where
  injectAllAPI = undefined

instance {-# OVERLAPS #-} (HasTSImpl a) => AllHasTSImpl '[a] where
  injectAllAPI _ = injectAPI (Proxy @a)

instance (HasTSImpl a,
          AllHasTSImpl xs,
          TSImplList (a ': xs) ~ (TSImpl a :+ TSImplList xs)
          ) => AllHasTSImpl (a ': xs) where
  injectAllAPI _ xs (x :+ rest) = injectAPI (Proxy @a) xs x >> injectAllAPI (Proxy @xs) xs rest

type family TSImplList (a :: [*]) where
  TSImplList '[] = TypeError ('Text "Can't solve for the implementation of an empty namespace")
  TSImplList '[x] = TSImpl x
  TSImplList (x ': xs) = TSImpl x :+ TSImplList xs

instance (HasTSImpl x, HasTSImpl y) => HasTSImpl (x :+ y) where
  type TSImpl (x :+ y) = (TSImpl x) :+ (TSImpl y)
  injectAPI _ ls (x :+ y) = injectAPI (Proxy @x) ls x >> injectAPI (Proxy @y) ls y

instance JsTypeable (RetTsType a) => HasTSImpl (Ret a) where
  type TSImpl (Ret a) = RetTsType a
-- We need a function name!
  injectAPI _ [] _ = undefined --injectChakra x ls ""
  injectAPI _ xs x = injectChakra x (init xs) (last xs)

instance (HasTSImpl xs, JsTypeable (t -> TSImpl xs)) => HasTSImpl (Arg n t :> xs) where
  type TSImpl (Arg n t :> xs) = t -> TSImpl xs
-- We need a function name!
  injectAPI _ [] _ = undefined --injectChakra x ls ""
  injectAPI _ xs x = injectChakra x (init xs) (last xs)

instance (JsTypeable (TSImpl xs), HasTSImpl xs, KnownSymbol n) => HasTSImpl (Fn n :> xs) where
  type TSImpl (Fn n :> xs) = TSImpl xs
  injectAPI _ xs = injectAPI (Proxy @xs) (xs ++ [symtext $ Proxy @n])

instance (KnownSymbol n, AllHasTSImpl xs) => HasTSImpl (NS n xs) where
  type TSImpl (NS n xs) = TSImplList xs
  injectAPI _ xs = injectAllAPI (Proxy @xs) (xs ++ [symtext $ Proxy @n])

-- Get .ts.d
genTS :: HasTSSpec spec => Proxy spec -> T.Text
genTS prox = tsSpec prox TopLevel

-- Typecheck and inject an API
injectNativeAPI :: (HasTSSpec bindings, HasTSImpl bindings) => Proxy bindings -> TSImpl bindings -> Chakra s ()
injectNativeAPI prox = injectAPI prox []
