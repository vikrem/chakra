{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Chakra where
import Raw
import qualified Language.C.Inline as C

C.include "ChakraCommon.h"
C.include "ChakraCoreVersion.h"
C.include "ChakraDebug.h"
C.include "ChakraCore.h"

someFunc :: IO ()
someFunc = do
  let at = JsRuntimeAttributeNone
  runtime <- jsCreateRuntime at ()
  context <- jsCreateContext runtime
  [C.block| void {
      JsRuntimeHandle runtime;
      JsContextRef context;
      JsValueRef result;
      JsCreateRuntime(JsRuntimeAttributeNone, NULL, &runtime);
      JsCreateContext(runtime, &context);
      JsSetCurrentContext(context);
      JsSetCurrentContext(JS_INVALID_REFERENCE);
      JsDisposeRuntime(runtime);
  } |]

  putStrLn "ye"
