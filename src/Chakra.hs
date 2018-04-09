{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Chakra (
  someFunc,
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

import Types
import Raw
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class

boundedWait :: IO a -> IO a
boundedWait f = asyncBound f >>= wait

someFunc :: IO ()
someFunc = do
  res <- runChakra $ do
    chakraEval "(function(){ return 3 + 15 + 12; })();"
  print res

-- This function should be the only thing to ever escape Chakra.
-- No occurrences of `unChakra` should exist, except here.
runChakra :: Chakra a -> IO a
runChakra MkChakra{..} = boundedWait $
  bracket
    setupChakra
    teardownChakra
    (const $ runResourceT unChakra)

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

chakraEval :: String -> Chakra JsValue
chakraEval = unsafeChakraEval >=> wrapJsValue

unsafeChakraEval :: String -> Chakra JsValueRef
unsafeChakraEval src = MkChakra $ lift $ do
      script <- jsCreateString src
      source <- jsCreateString "[runScript]"
      jsRun script 0 source JsParseScriptAttributeNone

injectChakra :: JsTypeable a => a -> [String] -> String -> Chakra ()
injectChakra fn namespaces name = do
  fnWrap <- cWrapper fn
  MkChakra $ lift $ do
    gObj <- jsGetGlobalObject
    nameSpace <- walkProps gObj namespaces
    nameObj <- jsCreateString name
    fnObj <- jsCreateFunction fnWrap ()
    jsSetIndexedProperty nameSpace nameObj fnObj
  where
    walkProps :: JsValueRef -> [String] -> IO JsValueRef
    walkProps obj [] = return obj
    walkProps obj (x:xs) = (do
        nameObj <- jsCreateString x
        nextObj <- jsGetIndexedProperty obj nameObj
        walkProps nextObj xs) `catchDeep` \(e :: SomeException) ->
      throwString $ "An exception occurred during function injection: " ++ displayException e
