{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Types where

import Raw
import Data.Aeson.Types
import qualified Data.Text as T

newtype Chakra a = MkChakra { unChakra :: IO a } deriving (Functor, Applicative, Monad)
newtype JsValue (t :: JsValueType) = MkJsValue JsValueRef

class ToJSValue a where
  type JsTag a :: JsValueType
  toJSValue :: a -> IO (JsValue (JsTag a))

class FromJSValue a where
  fromJSValue :: a -> IO (Maybe b)

instance ToJSValue String where
  type JsTag String = 'JsString
  toJSValue :: String -> IO (JsValue 'JsString)
  toJSValue s = MkJsValue <$> jsCreateString s

instance ToJSValue T.Text where
  type JsTag T.Text = 'JsString
  toJSValue = toJSValue . T.unpack
