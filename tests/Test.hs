{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

import Protolude hiding (catch)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Test.SmallCheck.Series
import Data.Aeson.Types hiding (Series)
import Data.Aeson hiding (Series)

import Control.Exception.Safe

import Data.String
import Data.IORef

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Chakra
import Data.Scientific
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "All Tests"
  [
      testGroup "Object identity"
    [ testRefl $ Proxy @Int
    , testRefl $ Proxy @Double
    , testRefl $ Proxy @Value
    , testRefl $ Proxy @T.Text
    , testRefl $ Proxy @String
    ]
    , testGroup "Calling into Haskell"
    [ testCase "JS->HS namespaced function call" hsNamespacedCall
    , testCase "JS->HS->JS callback usage" callbackTest
    , testCase "Stored callback usage" storedCallbackTest
    , testCase "Exceptions in a callback can pass through native function" callbackExc
    , testCase "Catch invalid Haskell call" canCatchBadCC
    ]
  ]

instance Monad m => Serial m T.Text where
  series = T.pack <$> (series :: Series m String)

instance (Eq k, Hashable k, Serial m k, Serial m v) => Serial m (HMS.HashMap k v) where
  series = HMS.fromList <$> (series :: Series m [(k,v)])

instance Serial m a => Serial m (V.Vector a) where
  series = V.fromList <$> (series :: Series m [a])

instance Monad m => Serial m Scientific where
  series = cons2 scientific

instance Monad m => Serial m Value

type CanGenRefl a = (Typeable a, Eq a, FromJSON a, ToJSON a, Serial IO a, Show a)

testRefl :: forall a. CanGenRefl a => Proxy a -> TestTree
testRefl _ = testGroup typeName [
    testProperty (typeName ++ "s can pass between HS and JS via bare function") $ typeRefl $ Proxy @a
  , testProperty (typeName ++ "s can pass between HS and JS via Promise") $ typePromiseRefl $ Proxy @a
  ]
  where
    typeName = show $ typeRep $ Proxy @a

typeRefl :: forall a. CanGenRefl a => Proxy a -> Property IO
typeRefl _ = forAll $ \(x :: a) -> monadic $ do
    let jsSerial = (T.decodeUtf8 . BSL.toStrict . encode $ x)
    let evalStr = "(" <> jsSerial <> ")"
    let hsFunc = liftIO $ return x :: HsFn a
    let callStr = "(f())"
    (Just direct_ref) <- fromJSValue <$> runChakra (chakraEval evalStr)
    (Just direct_call) <- fromJSValue <$> runChakra (injectChakra hsFunc [] "f" >> chakraEval callStr)
    return $ and $ fmap (== x) [direct_ref, direct_call]

typePromiseRefl :: forall a. CanGenRefl a => Proxy a -> Property IO
typePromiseRefl _ = forAll $ \(x :: a) -> monadic $ do
    reportVar <- newIORef Nothing
    let hsFunc = liftIO $ return x :: HsAsyncFn a
    let reportFunc = liftIO <$> writeIORef reportVar . Just :: a -> HsFn ()
    let callStr = "f().then((v) => {r(v)}).catch((e) => {throw e})"
    runChakra $ do
      injectChakra hsFunc [] "f"
      injectChakra reportFunc [] "r"
      chakraEval callStr
    (Just extract) <- readIORef reportVar
    return $ extract == x

hsNamespacedCall :: Assertion
hsNamespacedCall = do
  let js = "X.Y.Z.f(5, 10, 'hello');"
  let mkNs = "var X = {}; X.Y = {}; X.Y.Z = {};"
  v <- runChakra $ chakraEval mkNs >> injectChakra f ["X","Y","Z"] "f" >> chakraEval js
  let (Just val) = fromJSValue @Int v
  val @?= 20
  where
    f :: Int -> Int -> String -> HsFn Int
    f a b s = liftIO $ return $ a + b + length s

canCatchBadCC :: Assertion
canCatchBadCC = do
  death <- newIORef False
  (runChakra $ injectChakra func [] "f" >> chakraEval "f()") `catch`
    \(_ :: SomeException) -> writeIORef death True >> return undefined
  readIORef death >>= (@=?) True
  where
    func a b = liftIO $ return $ a + b :: HsFn Integer

callbackTest :: Assertion
callbackTest = do
  let js = "hsFn((x, y) => {return 5*x*y;});"
  v <- runChakra $ injectChakra hsFn [] "hsFn" >> chakraEval js
  let (Just val) = fromJSValue @Int v
  val @?= 30
  where
    hsFn :: JsCallback s -> HsFn Value
    hsFn cb = do
      v <- runCallback cb [toJSValue @Int 3, toJSValue @Int 2]
      return $ fromMaybe Null $ fromJSValue @Value v

storedCallbackTest :: Assertion
storedCallbackTest = do
    let js = "register((x, y) => {return 5*x*y;}); call();"
    v <- runChakra $ do
      ref <- liftIO $ newIORef Nothing
      injectChakra (register ref) [] "register"
      injectChakra (call ref) [] "call"
      chakraEval js
    let (Just val) = fromJSValue @Int v
    val @?= 30
  where
    register :: IORef (Maybe (JsCallback vm)) -> JsCallback vm -> HsFn Value
    register ref cb = do
      liftIO $ writeIORef ref $ Just cb
      return Null
    call :: IORef (Maybe (JsCallback vm)) -> HsFn Value
    call ref = do
      Just cb <- liftIO $ readIORef ref
      v <- runCallback cb [toJSValue @Int 3, toJSValue @Int 2]
      return $ fromMaybe Null $ fromJSValue @Value v

callbackExc :: Assertion
callbackExc = do
    let js = "register((x, y) => {throw new Error('hi');}); try { call(); } catch (e) { (() => {return e.message;})(); }"
    v <- runChakra $ do
      ref <- liftIO $ newIORef Nothing
      injectChakra (register ref) [] "register"
      injectChakra (call ref) [] "call"
      chakraEval js
    let (Just val) = fromJSValue @T.Text v
    val @?= "hi"
  where
    register :: IORef (Maybe (JsCallback vm)) -> JsCallback vm -> HsFn Value
    register ref cb = do
      liftIO $ writeIORef ref $ Just cb
      return Null
    call :: IORef (Maybe (JsCallback vm)) -> HsFn Value
    call ref = (do
      Just cb <- liftIO $ readIORef ref
      -- runCallback should throw a JsError here
      -- That JsError will be caught and rethrown at this function's call-site in js.
      v <- runCallback cb []
      return $ fromMaybe Null $ fromJSValue @Value v)
