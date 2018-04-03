{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Types (
  Chakra(..),
  JsValue,
  ToJSValue(..),
  FromJSValue(..),
  wrapJsValue,
  jsNull,
  jsUndefined
  ) where

import Raw
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- An IO exec environment that requires a bound, running js context to the current os thread
newtype Chakra a = MkChakra { unChakra :: IO a } deriving (Functor, Applicative, Monad)
-- JS Values that have no ties to any `JsValueRef`s
-- This is to let you read them without IO or a Chakra context.
-- allowing them to be pure and to escape Chakra.
newtype JsValue = MkJsValue BS8.ByteString deriving (Eq)

jsNull :: JsValue
jsNull = MkJsValue "null"

jsUndefined :: JsValue
jsUndefined = MkJsValue "undefined"

wrapJsValue :: JsValueRef -> Chakra JsValue
wrapJsValue ref = MkChakra $ do
    str_ref <- jsConvertValueToString ref
    s <- unsafeExtractJsString str_ref
    return $ MkJsValue $ BS8.pack s

instance Show JsValue where
  show :: JsValue -> String
  show (MkJsValue ref) = BS8.unpack ref

class ToJSValue a where
  toJSValue :: a -> JsValue

class FromJSValue a where
  fromJSValue :: JsValue -> Maybe a

instance ToJSON a => ToJSValue a where
  toJSValue = MkJsValue . BSL8.toStrict . encode

instance FromJSON a => FromJSValue a where
  fromJSValue (MkJsValue ref) = decodeStrict' ref
