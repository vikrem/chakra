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

import Data.Aeson.Types (Value)
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
  TsType Bool = "true | false"
  TsType UUID = "string"
  TsType UTCTime = "string"
  TsType (Maybe a) = AppendSymbol (TsType a) " | null"
  TsType () = "void"
  TsType (JsPromise a) = AppendSymbol "Promise<" (AppendSymbol (TsType a) ">")
  TsType [a] = (AppendSymbol (TsType a) "[]")
  TsType a = (GTsType (Rep a))

-- The final return type of a native function that returns an `a` to js
type family RetTsType a :: * where
  RetTsType (JsPromise a) = JsPromise a
  RetTsType typ = IO typ

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

-- API building

data NS (n :: Symbol) (a :: [*])
data Fn (name :: Symbol)
data Arg (name :: Symbol) (typ :: *)
data Ret (typ :: *)
data f1 :> f2
infixr 8 :>
data f1 :+ f2 = f1 :+ f2
infixr 9 :+

indent :: T.Text -> T.Text
indent s = T.unlines $ ("  " <> ) <$> T.lines s

class HasTSSpec (a :: b) where
  tsSpec :: Proxy a -> T.Text

class AllHasTSSpec (a :: [*]) where
  tsSpecList :: Proxy a -> [T.Text]

instance AllHasTSSpec '[] where
  tsSpecList _ = []

instance (HasTSSpec x, HasTSSpec y) => HasTSSpec (x :+ y) where
  tsSpec _ = tsSpec (Proxy @x) <> tsSpec (Proxy @y)

instance (HasTSSpec x, AllHasTSSpec xs) => AllHasTSSpec (x ': xs) where
  tsSpecList _ = (tsSpec (Proxy @x)) : (tsSpecList (Proxy @xs))

instance (AllHasTSSpec xs, KnownSymbol n) => HasTSSpec (NS n xs) where
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
    symtext (Proxy @(TsType t)) <>
    tsSpec (Proxy @xs)

instance KnownSymbol (TsType t) => HasTSSpec (Ret t) where
  tsSpec _ = "): " <> symtext (Proxy @(TsType t))

class HasTSImpl a where
  type TSImpl a :: *
-- Proxy needed because TSImpl isn't injective
  injectAPI :: Proxy a -> [T.Text] -> TSImpl a -> Chakra ()

class AllHasTSImpl (a :: [*]) where
  injectAllAPI :: Proxy a -> [T.Text] -> TSImplList a -> Chakra ()

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

genTS :: HasTSSpec spec => Proxy spec -> T.Text
genTS = tsSpec

injectNativeAPI :: (HasTSSpec bindings, HasTSImpl bindings) => Proxy bindings -> TSImpl bindings -> Chakra ()
injectNativeAPI prox = injectAPI prox []
