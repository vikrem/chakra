{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "ChakraCommon.h"
#include "ChakraCoreVersion.h"
#include "ChakraDebug.h"
#include "ChakraCore.h"
module Raw where

import Control.Exception.Safe
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

mNullPtr :: Ptr a -> IO (Ptr b)
mNullPtr = return . const Foreign.Ptr.nullPtr

nullPtr = Foreign.Ptr.nullPtr
nullFunPtr = Foreign.Ptr.nullFunPtr

withNullFunPtr :: a -> (FunPtr b -> IO c) -> IO c
withNullFunPtr _ f = f Foreign.Ptr.nullFunPtr

throwIfJsError :: CInt -> IO ()
throwIfJsError e = case (toEnum . fromIntegral $ e) of
  JsNoError -> pure () -- No exception
  _ -> do
    -- Grab the exception string from the runtime and throw it
    exc <- jsGetAndClearException
    excStrVal <- jsConvertValueToString exc
    excStr <- extractJsString excStrVal
    jsSetException exc -- Don't clear the exception
    throwString excStr -- Toss

{#enum JsErrorCode {} deriving (Eq, Show) #}
{#enum JsValueType {} deriving (Eq, Show) #}
{#enum JsRuntimeAttributes {} deriving (Eq, Show) #}
{#enum JsParseScriptAttributes {} deriving (Eq, Show) #}

type JsSourceContext = {#type JsSourceContext #}

jsEmptyContext :: JsContextRef
jsEmptyContext = Raw.nullPtr

{#pointer JsRuntimeHandle #}
{#pointer JsContextRef #}
{#pointer JsValueRef #}

{#fun JsCreateRuntime as ^
 {`JsRuntimeAttributes',
  withNullFunPtr* `()',
  alloca- `JsRuntimeHandle' peek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsCreateContext as ^
 {`JsRuntimeHandle',
  alloca- `JsContextRef' peek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsSetCurrentContext as ^
 {`JsContextRef'
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsCreateString as ^
 {`String'&,
  alloca- `JsValueRef' peek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsRun as ^
 {`JsValueRef',
  id `JsSourceContext',
  `JsValueRef',
  `JsParseScriptAttributes',
  alloca- `JsValueRef' peek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsConvertValueToString as ^
 {`JsValueRef',
  alloca- `JsValueRef' peek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsCopyString as ^
 {`JsValueRef',
  id `(Ptr CChar)',
  id `CULong',
  alloca- `CULong' peek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsDisposeRuntime as ^
 {`JsRuntimeHandle'
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsGetAndClearException as ^
 {alloca- `JsValueRef' peek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsSetException as ^
 {`JsValueRef'
} -> `JsErrorCode' throwIfJsError*-
 #}

-- val MUST BE A JS STRING !
extractJsString :: JsValueRef -> IO String
extractJsString val = do
  strLen <- jsCopyString val Raw.nullPtr 0
  allocaBytes (fromIntegral strLen) $ \p -> do
    jsCopyString val p strLen
    peekCStringLen (p, fromIntegral strLen)
