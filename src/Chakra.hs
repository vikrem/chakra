{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Chakra (
  Chakra,
  FromJSValue,
  HsAsyncFn,
  HsFn,
  JsCallback,
  JsTypeable,
  JsValue,
  SomeJsException,
  ToJSValue,
  asyncify,
  chakraEval,
  fromJSValue,
  injectChakra,
  jsNull,
  jsUndefined,
  runCallback,
  runChakra,
  syncify,
  toJSValue,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Data.IORef
import Data.Proxy
import Foreign.Ptr

import qualified Data.Text as T

import Raw
import Types

-- | Ensure that the IO action runs in a bound thread
boundedWait :: IO a -> IO a
boundedWait f = asyncBound f >>= wait

-- This function should be the only thing to ever escape Chakra.
-- No occurrences of `unChakra` should exist, except here.

-- | Executes a Chakra context in IO and returns the resulting JsValue --
-- so long as it's not bound to this free 'vm' param
runChakra :: (forall vm. Chakra vm a) -> IO a
runChakra chk = boundedWait $ runResourceT $ do
  allocate
    setupChakra
    teardownChakra
  unChakra chk

setupChakra :: IO JsRuntimeHandle
setupChakra = do
  runtime <- jsCreateRuntime JsRuntimeAttributeNone ()
  context <- jsCreateContext runtime
  jsSetCurrentContext context
  return runtime

teardownChakra :: JsRuntimeHandle -> IO ()
teardownChakra rt = do
  jsSetCurrentContext jsEmptyContext
  jsDisposeRuntime rt

pushPromise :: TChan JsValueRef -> JsValueRef -> Ptr () -> IO ()
pushPromise chan ref _ = jsAddRef ref >> atomically (writeTChan chan ref)

evalPromises :: TChan JsValueRef -> IORef JsValue -> Chakra s ()
evalPromises chan ref = MkChakra $ lift $ atomically (tryReadTChan chan) >>= \case
  Nothing -> pure ()
  Just val -> do
    gObj <- jsGetGlobalObject
    barejsval <- jsCallFunction val [gObj]
    wrappedval <- runResourceT $ unChakra $ wrapJsValue barejsval
    liftIO $ writeIORef ref wrappedval
    liftIO $ jsRelease barejsval
    runResourceT $ unChakra $ evalPromises chan ref

-- | Evaluate js code
--
-- >>> runChakra $ chakraEval "5 + 3"
-- 8
chakraEval :: T.Text -> Chakra vm JsValue
chakraEval t = do
  promiseQueue <- liftIO $ atomically newTChan
  (promiseKey, promisePtr) <- MkChakra $ allocate
    (mkJsPromiseCallback $ pushPromise promiseQueue)
    freeJsPromiseCallback
  liftIO $ jsSetPromiseContinuationCallback promisePtr ()
  ref <- unsafeChakraEval t >>= wrapJsValue >>= liftIO . newIORef
  evalPromises promiseQueue ref
  release promiseKey
  liftIO $ jsSetPromiseContinuationCallback Raw.nullFunPtr ()
  liftIO $ readIORef ref

unsafeChakraEval :: T.Text -> Chakra vm JsValueRef
unsafeChakraEval src = MkChakra $ lift $ do
      script <- jsCreateString src
      source <- jsCreateString "[runScript]"
      jsRun script 0 source JsParseScriptAttributeNone

-- | Inject a haskell function into a js environment
--
-- >>> runChakra $ injectChakra (\fn -> return (fn ++ "!") :: IO String) [] "f" >>  chakraEval "f('3');"
-- "3!"
injectChakra :: JsTypeable s a => a -> [T.Text] -> T.Text -> Chakra s ()
injectChakra fn namespaces name = do
  fnWrap <- cWrapper (Proxy :: Proxy s) fn
  MkChakra $ lift $ do
    gObj <- jsGetGlobalObject
    nameSpace <- unsafeWalkProps gObj namespaces
    nameObj <- jsCreateString name
    fnObj <- jsCreateFunction fnWrap ()
    jsSetIndexedProperty nameSpace nameObj fnObj

unsafeWalkProps :: JsValueRef -> [T.Text] -> IO JsValueRef
unsafeWalkProps obj [] = return obj
unsafeWalkProps obj (x:xs) = (do
    nameObj <- jsCreateString x
    nextObj <- jsGetIndexedProperty obj nameObj
    unsafeWalkProps nextObj xs) `catchDeep` \(e :: SomeJsException) ->
  throwString $ "An exception occurred during function injection: " ++ displayException e

-- | Runs a callback, but is unsafe as it ignores the bound `s` on the callback
unsafeRunCallback :: MonadIO m => JsCallback vm -> [JsValue] -> m JsValue
unsafeRunCallback (MkJsCallback ref) args = liftIO $ do
  argRefs <- sequence $ unsafeMakeJsValueRef <$> args
  retVal <- jsGetGlobalObject >>= \u -> jsCallFunction ref $ u:argRefs
  unsafeWrapJsValue retVal

-- | Runs a callback bound to the `vm` of the vm
-- If the js callback throws an exception, that exception will be of type `SomeJsException`, and will be rethrown at the site of the native function call if not caught in Haskell.
runCallback :: JsCallback s -> [JsValue] -> HsFn JsValue
runCallback = unsafeRunCallback
