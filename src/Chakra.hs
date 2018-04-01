{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Chakra (
  someFunc,
  runChakra,
  chakraEval,
  Chakra
              )
where

import Raw
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class

newtype Chakra a = MkChakra { unChakra :: IO a } deriving (Functor, Applicative, Monad)

data JsValue (t :: JsValueType) = MkJsValue JsValueRef

boundedWait :: IO a -> IO a
boundedWait f = asyncBound f >>= wait

someFunc :: IO ()
someFunc = do
  s <- runChakra $ chakraEval "(function(){ return 3 + 15 + 12; })();"
  putStrLn s

runChakra :: Chakra a -> IO a
runChakra MkChakra{..} = boundedWait $
  bracket
    setupChakra
    teardownChakra
    (const unChakra)

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

chakraEval :: String -> Chakra String
chakraEval src = MkChakra $ do
    script <- jsCreateString src
    source <- jsCreateString "[runScript]"
    ret <- jsRun script 0 source JsParseScriptAttributeNone
    jsConvertToString ret

jsConvertToString :: JsValueRef -> IO String
jsConvertToString ref = do
  str_ref <- jsConvertValueToString ref
  unsafeExtractJsString str_ref
