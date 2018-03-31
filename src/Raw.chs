{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "ChakraCommon.h"
#include "ChakraCoreVersion.h"
#include "ChakraDebug.h"
#include "ChakraCore.h"
module Raw where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

mNullPtr :: Ptr a -> IO (Ptr b)
mNullPtr = return . const nullPtr

withNullFunPtr :: a -> (FunPtr b -> IO c) -> IO c
withNullFunPtr _ f = f nullFunPtr

throwIfJsError :: CInt -> IO ()
throwIfJsError e = case (toEnum . fromIntegral $ e) of
  a@JsNoError -> print a
  a -> print a >> error "fk"

{#enum JsErrorCode {} deriving (Eq, Show) #}
{#enum JsValueType {} deriving (Eq, Show) #}
{#enum JsRuntimeAttributes {} deriving (Eq, Show) #}

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
