{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Types (
  Chakra(..),
  JsValue,
  ToJSValue(..),
  FromJSValue(..),
  JsTypeable(..),
  wrapJsValue,
  jsNull,
  jsUndefined
  ) where

import Raw
import Data.Aeson
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Foreign.Marshal.Array

import GHC.TypeLits
import Data.Proxy

-- An IO exec environment that requires a bound, running js context to the current os thread
newtype Chakra a = MkChakra { unChakra :: ResIO a } deriving (Functor, Applicative, Monad)
-- JS Values that have no ties to any `JsValueRef`s
-- This is to let you read them without IO or a Chakra context.
-- allowing them to be pure and to escape Chakra.
newtype JsValue = MkJsValue BS8.ByteString deriving (Eq)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

jsNull :: JsValue
jsNull = MkJsValue "null"

jsUndefined :: JsValue
jsUndefined = MkJsValue "undefined"

unsafeWrapJsValue :: JsValueRef -> IO JsValue
unsafeWrapJsValue ref = do
  -- Fetch JSON.serialize
  script <- jsCreateString "(function () { return JSON.stringify; })();"
  source <- jsCreateString "[wrapJsValue]"
  -- Run it against our ref
  -- First arg is 'this', so we set it to undefined
  stringify <- jsRun script 1 source JsParseScriptAttributeNone
  mybstr <- jsGetUndefinedValue >>= \u -> jsCallFunction stringify [u, ref]
  -- This could return undefined / null, so cast just to be safe
  serialObj <- jsConvertValueToString mybstr
  -- Fetch str
  s <- unsafeExtractJsString serialObj
  return $ MkJsValue $ BS8.pack s

wrapJsValue :: JsValueRef -> Chakra JsValue
wrapJsValue ref = MkChakra $ lift $ unsafeWrapJsValue ref

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

-- Getting funcs into chakra env
data JsFnTypelist (inpTypes :: [*]) (outType :: *)

type family AddInpType n m where
  AddInpType n (JsFnTypelist i o) = JsFnTypelist (n:i) o
type family GetInpTypeLen xs :: Nat where
  GetInpTypeLen (JsFnTypelist '[] o) = 0

class JsTypeable a where
  type JsType a
  cWrapper :: a -> Chakra JsNativeFunction
  cWrapper fn = MkChakra $ do
    -- Register GC for funptr
    (_, ptr) <- allocate
      (mkJsNativeFunction $ dropFirstArg $ cBare fn)
      freeJsNativeFunction
    return ptr
  cBare :: a -> JsUnwrappedNativeFunction
-- The first argument passed to a native function is 'this'.
-- This is used to drop it

dropFirstArg :: JsUnwrappedNativeFunction -> JsUnwrappedNativeFunction
dropFirstArg f = \a b arr argCount c ->
  f a b (advancePtr arr 1) (argCount - 1) c

instance ToJSValue a => JsTypeable (IO a) where
  type JsType (IO a) = JsFnTypelist '[] a
  -- ignore all args, we just want to return a value
  cBare r = \_ _ _ _ _ -> do
    (MkJsValue json) <- toJSValue <$> r
    jsons <- jsCreateString $ BS8.unpack json
    -- Fetch JSON.parse
    script <- jsCreateString "(function () { return JSON.parse; })();"
    source <- jsCreateString "[unwrapJsValue]"
    parsefn <- jsRun script 1 source JsParseScriptAttributeNone
    -- parse and return resulting object
    jsGetUndefinedValue >>= \u -> jsCallFunction parsefn [u, jsons]

instance (FromJSValue a, JsTypeable b) => JsTypeable (a -> b) where
  type JsType (a -> b) = AddInpType a (JsType b)
  cBare fn = \callee isConstruct argArr argCount cbState -> do
    headParam <- safeHead <$> peekArray (fromIntegral argCount) argArr
    case headParam of
      Nothing -> throwJsError "Expecting further arguments to function. Not enough provided?"
      Just p -> do
        conv <- fromJSValue @a <$> unsafeWrapJsValue p
        case conv of
          Nothing -> throwJsError "Conversion error when reading a function argument"
          Just c -> cBare (fn c) callee isConstruct (advancePtr argArr 1) (argCount - 1) cbState
    where
      throwJsError str = do
        errMsg <- jsCreateString $ "Error in native call: " ++ str
        err <- jsCreateError errMsg
        jsSetException err
        jsGetUndefinedValue
