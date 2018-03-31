{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

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

mNullFunPtr :: Ptr a -> IO (FunPtr b)
mNullFunPtr = return . const nullFunPtr

{#enum JsErrorCode {} deriving (Eq, Show) #}
{#enum JsValueType {} deriving (Eq, Show) #}
{#enum JsRuntimeAttributes {} deriving (Eq, Show) #}

{#pointer JsRuntimeHandle #}

{#fun JsCreateRuntime as ^
 {`JsRuntimeAttributes',
  id `FunPtr (FunPtr (Ptr () -> IO ()) -> Ptr () -> IO CInt)',
  alloca- `JsRuntimeHandle' peek*
} -> `()'
 #}
