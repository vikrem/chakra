{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include "ChakraCommon.h"
#include "ChakraCoreVersion.h"
#include "ChakraDebug.h"
#include "ChakraCore.h"
module Raw where

import Control.Monad ((>=>))
import Data.Typeable (cast)

import Control.Concurrent
import Control.Exception.Safe
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Foreign as T

{#enum JsErrorCode {} deriving (Eq, Show) #}
{#enum JsValueType {} deriving (Eq, Show) #}
{#enum JsRuntimeAttributes {} deriving (Eq, Show) #}
{#enum JsParseScriptAttributes {} deriving (Eq, Show) #}

-- | Exception type to capture exceptions raised within JS.
data SomeJsException = forall e. Exception e => SomeJsException e

instance Show SomeJsException where
  show (SomeJsException e) = show e

instance Exception SomeJsException
data JsErrorException = JsErrorException JsErrorCode T.Text deriving (Show)

instance Exception JsErrorException where
  toException = toException . SomeJsException
  fromException x = fromException x >>= \(SomeJsException e) -> cast e

mNullPtr :: Ptr a -> IO (Ptr b)
mNullPtr = return . const Foreign.Ptr.nullPtr

nullPtr = Foreign.Ptr.nullPtr
nullFunPtr = Foreign.Ptr.nullFunPtr

withNullFunPtr :: a -> (FunPtr b -> IO c) -> IO c
withNullFunPtr _ f = f Foreign.Ptr.nullFunPtr

withNullPtr :: a -> (Ptr b -> IO c) -> IO c
withNullPtr _ f = f Foreign.Ptr.nullPtr

withText :: T.Text -> ((Ptr CChar, CULong) -> IO a) -> IO a
withText t fn = T.withCStringLen t $ \(p, len) -> fn (p, fromIntegral len)

-- use [a] for C calling convention (*a, size_t len)
splatArray :: Storable a => [a] -> ((Ptr a, CUShort) -> IO b) -> IO b
splatArray arr fn = do
  withArrayLen arr $ \len pt -> fn (pt, fromIntegral len)

peekJsType :: (Ptr CInt) -> IO JsValueType
peekJsType = peek >=> return . toEnum . fromIntegral

throwIfJsError :: CInt -> IO ()
throwIfJsError e = case (toEnum . fromIntegral $ e) of
  JsNoError -> pure () -- No exception
  _ -> do
    -- Grab the exception string from the runtime and throw it
    (errCode, exc) <- jsGetAndClearException
    case errCode of
      JsNoError -> do
        excStrVal <- jsConvertValueToString exc
        excStr <- unsafeExtractJsString excStrVal
        jsSetException exc -- Don't clear the exception
        throw $ JsErrorException errCode excStr -- Toss
      JsErrorInvalidArgument -> do
        -- The runtime is actually not in an exception state, but we made a bad func call
        throwString $ "An error code was raised during a Chakra function call: "
          ++ show @JsErrorCode (toEnum . fromIntegral $ e)
      _ -> do
        throwString $ "An exception occurred during the handling of an earlier exception: " ++ show errCode

type JsSourceContext = {#type JsSourceContext #}
type JsNativeFunction = {#type JsNativeFunction #}
type JsUnwrappedNativeFunction =
      (JsValueRef -- callee
     -> CInt -- isConstructCall
     -> Ptr JsValueRef -- Arguments
     -> CUShort -- Arg len
     -> Ptr () -- Callback State
     -> IO (JsValueRef)) -- Retval

foreign import ccall "wrapper" mkJsNativeFunction :: JsUnwrappedNativeFunction -> IO JsNativeFunction

type JsPromiseContinuationCallback = {#type JsPromiseContinuationCallback #}
type JsUnwrappedPromiseContinuationCallback =
  (JsValueRef -- Task
  -> Ptr () -- Void data
  -> IO ())


foreign import ccall "wrapper" mkJsPromiseCallback :: JsUnwrappedPromiseContinuationCallback -> IO JsPromiseContinuationCallback

freeJsNativeFunction :: JsNativeFunction -> IO ()
freeJsNativeFunction = freeHaskellFunPtr

freeJsPromiseCallback :: JsPromiseContinuationCallback -> IO ()
freeJsPromiseCallback = freeHaskellFunPtr

jsEmptyContext :: JsContextRef
jsEmptyContext = Raw.nullPtr


-- jsAlloca :: (Ptr JsValueRef -> IO b) -> IO b
-- jsAlloca f = alloca $ \p -> (peek p >>= jsAddRef) >> f p

-- | Peek at a JsValue. This JsValue is managed by Haskell.
-- We add a reference to prevent the Chakra GC from touching it.
jsPeek :: Ptr JsValueRef -> IO JsValueRef
jsPeek p = peek p >>= \ref -> jsAddRef ref >> return ref

{#pointer JsRuntimeHandle #}
{#pointer JsContextRef #}
{#pointer JsValueRef #}

-- existentialize on s to keep refs from escaping scope
newtype JsSafeRef s = MkSafeRef JsValueRef
unwrapSafeRef :: JsSafeRef s -> JsValueRef
unwrapSafeRef (MkSafeRef r) = r

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
 {withText* `T.Text'&,
  alloca- `JsValueRef' jsPeek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsRun as ^
 {`JsValueRef',
  id `JsSourceContext',
  `JsValueRef',
  `JsParseScriptAttributes',
  alloca- `JsValueRef' jsPeek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsConvertValueToString as ^
 {`JsValueRef',
  alloca- `JsValueRef' jsPeek*
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

{#fun JsGetValueType as ^
 {`JsValueRef',
  alloca- `JsValueType' peekJsType*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsGetUndefinedValue as ^
 {alloca- `JsValueRef' jsPeek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsGetAndClearException as ^
 {alloca- `JsValueRef' jsPeek*
} -> `JsErrorCode'
 #}

{#fun JsSetException as ^
 {`JsValueRef'
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsCallFunction as ^
 {`JsValueRef',
  splatArray* `[JsValueRef]'&,
  alloca- `JsValueRef' jsPeek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsSetIndexedProperty as ^
 {`JsValueRef',
  `JsValueRef',
  `JsValueRef'
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsGetIndexedProperty as ^
 {`JsValueRef',
  `JsValueRef',
  alloca- `JsValueRef' jsPeek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsGetGlobalObject as ^
 {alloca- `JsValueRef' jsPeek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsCreateFunction as ^
 {id `JsNativeFunction',
  withNullPtr* `()',
  alloca- `JsValueRef' jsPeek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsCreateError as ^
 {`JsValueRef',
  alloca- `JsValueRef' jsPeek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsAddRef as ^
 {`JsValueRef',
  alloca- `CUInt' peek*
} -> `JsErrorCode' --throwIfJsError*- Is this safe to remove..?
 #}

{#fun JsRelease as ^
 {`JsValueRef',
  alloca- `CUInt' peek*
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsSetPromiseContinuationCallback as ^
 {id `JsPromiseContinuationCallback',
  withNullPtr* `()'
} -> `JsErrorCode' throwIfJsError*-
 #}

{#fun JsCreatePromise as ^
 {alloca- `JsValueRef' jsPeek*,
  alloca- `JsValueRef' jsPeek*,
  alloca- `JsValueRef' jsPeek*
} -> `JsErrorCode' throwIfJsError*-
 #}

-- val MUST BE A JS STRING !
unsafeExtractJsString :: JsValueRef -> IO T.Text
unsafeExtractJsString val = do
  strLen <- jsCopyString val Raw.nullPtr 0
  allocaBytes (fromIntegral strLen) $ \p -> do
    jsCopyString val p strLen
    T.peekCStringLen (p, fromIntegral strLen)
