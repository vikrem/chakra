{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types (
  Chakra(..),
  FromJSValue(..),
  HsAsyncFn(..),
  HsFn(..),
  JsCallback(..),
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
import Data.Proxy
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

-- | An environment that Chakra work is computed in
-- Returns an `a` and is bound to a free `vm` param
newtype Chakra vm a = MkChakra { unChakra :: ResIO a } deriving (Functor, Applicative, Monad, MonadIO)

-- | JS Values, used by Chakra.
-- ToJSValue / FromJSValue can be used to move between Haskell values and JsValues
newtype JsValue = MkJsValue BS8.ByteString

-- | A handle to a JS function in a Chakra context.
-- It is bound to the 'vm', and cannot escape its Chakra context.
newtype JsCallback vm = MkJsCallback JsValueRef

-- | A wrapper around an IO action, that will run asynchronously.
-- These functions are exposed as Promise objects in Chakra.
newtype HsAsyncFn a = MkNativeAsyncFn { unAsyncFn :: IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

-- | A wrapper around an IO action, that will run synchronously
newtype HsFn a = MkNativeFn { unSyncFn :: IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

-- | Transform a synchronous hs function to an asynchronous one
asyncify :: HsFn a -> HsAsyncFn a
asyncify (MkNativeFn fn) = MkNativeAsyncFn fn

-- | Transform an asynchronous function to a synchronous one
syncify :: HsAsyncFn a -> HsFn a
syncify (MkNativeAsyncFn fn) = MkNativeFn fn

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

-- | A typeclass to cover functions that can be injected
-- JsTypeable vm a means that `a` can be injected into the VM's environment
-- the `vm` parameter is bound to the `Chakra vm` that it is used in
class JsTypeable vm a where
  -- A computation within the Chakra environment, that injects the function
  cWrapper :: Proxy vm -> a -> Chakra vm JsNativeFunction
  cWrapper vm fn = MkChakra $ do
    -- Register GC for funptr
    (_, ptr) <- allocate
      (mkJsNativeFunction $ wrapException $ dropFirstArg $ cBare vm fn)
      freeJsNativeFunction
    return ptr
  -- Transform `a` to a C-function that ChakraCore can accept
  cBare :: Proxy vm -> a -> JsUnwrappedNativeFunction

-- The first argument passed to a native function is 'this'.
-- This is used to drop it
dropFirstArg :: JsUnwrappedNativeFunction -> JsUnwrappedNativeFunction
dropFirstArg f = \a b arr argCount c ->
  f a b (advancePtr arr 1) (argCount - 1) c

-- Safely capture native exceptions and return them to js
wrapException :: JsUnwrappedNativeFunction -> JsUnwrappedNativeFunction
wrapException f = \a b c d e -> raiseUnknown $ raiseJs $ f a b c d e
  where
    -- The error could be any error at all
    raiseUnknown f = f `catchDeep` \(ex :: SomeException) ->
      unsafeThrowJsError $ T.pack $ displayException ex
    -- The error is an exception in the JS runtime (e.g. throw kwd in js was called)
    raiseJs f = f `catchDeep` \(_ :: JsErrorException) -> jsGetUndefinedValue

-- | A function that simply returns an `a`, and that `a` can be converted to a JsValue, is an injectible function
instance ToJSValue a => JsTypeable s (HsFn a) where
  -- ignore all args, we just want to return a value
  cBare _ r = \_ _ _ _ _ ->
    unSyncFn r >>= unsafeMakeJsValueRef . toJSValue

-- | An async function that simply returns JsValue-able `a`, can be injected as a function returning a Promise object
instance ToJSValue a => JsTypeable s (HsAsyncFn a) where
  cBare _ r = \_ _ _ _ _ -> do
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

-- | Allow injectible functions to consumes a JsCallback, bound to the current vm
instance {-# INCOHERENT #-} (JsTypeable vm b, vm1 ~ vm) => JsTypeable vm (JsCallback vm1 -> b) where
  cBare :: Proxy vm -> (JsCallback vm -> b) -> JsUnwrappedNativeFunction
  cBare px fn = \callee isConstruct argArr argCount cbState -> do
    headParam <- safeHead <$> peekArray (fromIntegral argCount) argArr
    case headParam of
      Nothing -> unsafeThrowJsError "Expecting further arguments to function. Not enough provided?"
      Just c -> do
        typ <- jsGetValueType c
        when (typ /= JsFunction) $
          void $ unsafeThrowJsError $ "Expecting a JS function as an argument, found " <> (T.pack $ show c)
        jsAddRef c -- Callbacks may be stored by hs and used later.. this needs a better fix
        cBare px (fn $ MkJsCallback c) callee isConstruct (advancePtr argArr 1) (argCount - 1) cbState

-- | Allow injectible functions to consume values that have a FromJSValue instance
instance (FromJSValue a, JsTypeable vm b, vm ~ vm1) => JsTypeable vm1 (a -> b) where
  cBare :: Proxy vm -> (a -> b) -> JsUnwrappedNativeFunction
  cBare px fn = \callee isConstruct argArr argCount cbState -> do
    headParam <- safeHead <$> peekArray (fromIntegral argCount) argArr
    case headParam of
      Nothing -> unsafeThrowJsError "Expecting further arguments to function. Not enough provided?"
      Just p -> do
        (conv :: Maybe a) <- fromJSValue <$> unsafeWrapJsValue p
        case conv of
          Nothing -> unsafeThrowJsError "Conversion error when reading a function argument"
          Just c -> cBare px (fn c) callee isConstruct (advancePtr argArr 1) (argCount - 1) cbState

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
