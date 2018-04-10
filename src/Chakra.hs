{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Chakra (
  runChakra,
  chakraEval,
  injectChakra,
  fromJSValue,
  toJSValue,
  jsNull,
  jsUndefined,
  Chakra
              )
where


import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Data.IORef
import Foreign.Ptr
import qualified Data.Text as T

import Raw
import Types

boundedWait :: IO a -> IO a
boundedWait f = asyncBound f >>= wait

-- This function should be the only thing to ever escape Chakra.
-- No occurrences of `unChakra` should exist, except here.
runChakra :: Chakra JsValue -> IO JsValue
runChakra chk = boundedWait $ runResourceT $ do
  allocate setupChakra teardownChakra
  promiseQueue <- lift $ atomically newTChan
  (_, promisePtr) <- allocate
    (mkJsPromiseCallback $ pushPromise promiseQueue)
    freeJsPromiseCallback
  lift $ jsSetPromiseContinuationCallback promisePtr ()
  r <- unChakra chk >>= \v -> lift $ newIORef v
  unChakra $ evalPromises promiseQueue r
  lift $ readIORef r

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

evalPromises :: TChan JsValueRef -> IORef JsValue -> Chakra ()
evalPromises chan ref = MkChakra $ lift $ atomically (tryReadTChan chan) >>= \case
  Nothing -> pure ()
  Just val -> do
    gObj <- jsGetGlobalObject
    barejsval <- jsCallFunction val [gObj]
    wrappedval <- runResourceT $ unChakra $ wrapJsValue barejsval
    liftIO $ writeIORef ref wrappedval
    liftIO $ jsRelease barejsval
    runResourceT $ unChakra $ evalPromises chan ref

chakraEval :: T.Text -> Chakra JsValue
chakraEval = unsafeChakraEval >=> wrapJsValue

unsafeChakraEval :: T.Text -> Chakra JsValueRef
unsafeChakraEval src = MkChakra $ lift $ do
      script <- jsCreateString src
      source <- jsCreateString "[runScript]"
      jsRun script 0 source JsParseScriptAttributeNone

injectChakra :: JsTypeable a => a -> [T.Text] -> T.Text -> Chakra ()
injectChakra fn namespaces name = do
  fnWrap <- cWrapper fn
  MkChakra $ lift $ do
    gObj <- jsGetGlobalObject
    nameSpace <- walkProps gObj namespaces
    nameObj <- jsCreateString name
    fnObj <- jsCreateFunction fnWrap ()
    jsSetIndexedProperty nameSpace nameObj fnObj
  where
    walkProps :: JsValueRef -> [T.Text] -> IO JsValueRef
    walkProps obj [] = return obj
    walkProps obj (x:xs) = (do
        nameObj <- jsCreateString x
        nextObj <- jsGetIndexedProperty obj nameObj
        walkProps nextObj xs) `catchDeep` \(e :: SomeException) ->
      throwString $ "An exception occurred during function injection: " ++ displayException e
