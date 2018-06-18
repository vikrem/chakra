{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types (
  Chakra(..),
  FromJSValue(..),
  HsAsyncFn(..),
  HsFn(..),
  JsCallback(..),
  JsFnTypelist,
  JsType,
  JsTypeable(..),
  JsValue,
  ToJSValue(..),
  asyncify,
  jsNull,
  jsUndefined,
  syncify,
  unsafeMakeJsValueRef,
  unsafeWrapJsValue,
  wrapJsValue,
  ) where

import Raw
import Data.Aeson
import Data.Monoid ((<>))
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Exception.Safe

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Foreign.Marshal.Array

import GHC.TypeLits

-- An IO exec environment that requires a bound, running js context to the current os thread
-- `s` is used to bind to the current VM
newtype Chakra s a = MkChakra { unChakra :: ResIO a } deriving (Functor, Applicative, Monad, MonadIO)

-- JS Values that represent more than what an Aeson Value represents
-- This is to let you read them without IO or a Chakra context.
-- allowing them to be pure and to escape Chakra.
newtype JsValue = MkJsValue BS8.ByteString

-- A handle to a js function in Chakra
-- Bound to the 's'
newtype JsCallback s = MkJsCallback JsValueRef

-- | A wrapper around an IO action, that will run asynchronously
newtype HsAsyncFn s a = MkNativeAsyncFn { unAsyncFn :: IO a } deriving (Functor, Applicative)

deriving instance Monad (HsAsyncFn s)
deriving instance MonadIO (HsAsyncFn s)

-- | A wrapper around an IO action, that will run synchronously
newtype HsFn s a = MkNativeFn { unSyncFn :: IO a } deriving (Functor, Applicative)

deriving instance Monad (HsFn s)
deriving instance MonadIO (HsFn s)

-- Natural transformations
syncify :: HsFn s a -> HsAsyncFn s a
syncify (MkNativeFn fn) = MkNativeAsyncFn fn

asyncify :: HsAsyncFn s a -> HsFn s a
asyncify (MkNativeAsyncFn fn) = MkNativeFn fn

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

void :: Monad m => m a -> m ()
void f = f >> pure ()

-- | a JsValue that represents `null` in Javascript
jsNull :: JsValue
jsNull = MkJsValue "null"

-- | a JsValue that represents `undefined` in Javascript
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
  return $ MkJsValue $ T.encodeUtf8 s

wrapJsValue :: JsValueRef -> Chakra s JsValue
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

type family JsType a where
  JsType (HsFn s a) = JsFnTypelist '[] (HsFn s a)
  JsType (HsAsyncFn s a) = JsFnTypelist '[] (HsAsyncFn s a)
  JsType (a -> b) = AddInpType a (JsType b)

class JsTypeable a where
  cWrapper :: a -> Chakra s JsNativeFunction
  cWrapper fn = MkChakra $ do
    -- Register GC for funptr
    (_, ptr) <- allocate
      (mkJsNativeFunction $ wrapException $ dropFirstArg $ cBare fn)
      freeJsNativeFunction
    return ptr
  cBare :: a -> JsUnwrappedNativeFunction

-- The first argument passed to a native function is 'this'.
-- This is used to drop it
dropFirstArg :: JsUnwrappedNativeFunction -> JsUnwrappedNativeFunction
dropFirstArg f = \a b arr argCount c ->
  f a b (advancePtr arr 1) (argCount - 1) c

-- Safely capture native exceptions and return them to js
wrapException :: JsUnwrappedNativeFunction -> JsUnwrappedNativeFunction
wrapException f = \a b c d e -> f a b c d e `catchDeep` \(ex :: SomeException) -> do
  unsafeThrowJsError $ T.pack $ displayException ex

instance ToJSValue a => JsTypeable (HsFn s a) where
  -- ignore all args, we just want to return a value
  cBare r = \_ _ _ _ _ ->
    unSyncFn r >>= unsafeMakeJsValueRef . toJSValue

instance ToJSValue a => JsTypeable (HsAsyncFn s a) where
  cBare r = \_ _ _ _ _ -> do
    (promise, accept, reject) <- jsCreatePromise
    sequence_ $ jsAddRef <$> [promise, accept, reject]
    gObj <- jsGetGlobalObject
    evalPromise gObj accept
      `catchAny` \(e :: SomeException) -> do
        ref <- unsafeMakeJsValueRef . toJSValue $ displayException e
        void $ jsCallFunction reject [gObj, ref]
      `finally` (sequence_ $ jsRelease <$> [promise, accept, reject])
    return promise
    where
      evalPromise gObj accept = unAsyncFn r >>= \val -> do
          ref <- unsafeMakeJsValueRef $ toJSValue val
          void $ jsCallFunction accept [gObj, ref]

-- TODO: Is this the only way to get callbacks to be treated differently?
instance {-# INCOHERENT #-} (JsTypeable b) => JsTypeable (JsCallback s -> b) where
  cBare :: (JsCallback s -> b) -> JsUnwrappedNativeFunction
  cBare fn = \callee isConstruct argArr argCount cbState -> do
    headParam <- safeHead <$> peekArray (fromIntegral argCount) argArr
    case headParam of
      Nothing -> unsafeThrowJsError "Expecting further arguments to function. Not enough provided?"
      Just c -> do
        typ <- jsGetValueType c
        when (typ /= JsFunction) $
          void $ unsafeThrowJsError $ "Expecting a JS function as an argument, found " <> (T.pack $ show c)
        cBare (fn $ MkJsCallback c) callee isConstruct (advancePtr argArr 1) (argCount - 1) cbState

instance (FromJSValue a, JsTypeable b) => JsTypeable (a -> b) where
  cBare :: (a -> b) -> JsUnwrappedNativeFunction
  cBare fn = \callee isConstruct argArr argCount cbState -> do
    headParam <- safeHead <$> peekArray (fromIntegral argCount) argArr
    case headParam of
      Nothing -> unsafeThrowJsError "Expecting further arguments to function. Not enough provided?"
      Just p -> do
        (conv :: Maybe a) <- fromJSValue <$> unsafeWrapJsValue p
        case conv of
          Nothing -> unsafeThrowJsError "Conversion error when reading a function argument"
          Just c -> cBare (fn c) callee isConstruct (advancePtr argArr 1) (argCount - 1) cbState

unsafeThrowJsError :: T.Text -> IO JsValueRef
unsafeThrowJsError str = do
  errMsg <- jsCreateString $ "Error in native call: " <> str
  err <- jsCreateError errMsg
  jsSetException err
  jsGetUndefinedValue

unsafeMakeJsValueRef :: JsValue -> IO JsValueRef
unsafeMakeJsValueRef (MkJsValue json) = do
    jsons <- jsCreateString $ T.decodeUtf8 json
    -- Fetch JSON.parse
    script <- jsCreateString "(function () { return JSON.parse; })();"
    source <- jsCreateString "[unwrapJsValue]"
    parsefn <- jsRun script 1 source JsParseScriptAttributeNone
    -- parse and return resulting object
    jsGetUndefinedValue >>= \u -> jsCallFunction parsefn [u, jsons]
