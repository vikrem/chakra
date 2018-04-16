{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

import Protolude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Test.SmallCheck.Series
import Data.Aeson.Types hiding (Series)
import Data.Aeson hiding (Series)

import Control.Exception.Safe
import Control.Concurrent.Async

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
    , testCase "Catch invalid Haskell call" canCatchBadCC
    ]
  ]

instance Monad m => Serial m T.Text where
  series = T.pack <$> (series :: Series m String)

instance (Eq k, Hashable k, Serial m k, Serial m v) => Serial m (HMS.HashMap k v) where
  series = do
    key <- series
    value <- series
    let element = return (key,value)
    d <- getDepth
    ls <- listM d element
    return $ HMS.fromList ls

instance Serial m a => Serial m (V.Vector a) where
  series = do
    value <- series
    d <- getDepth
    let value' = return value
    ls <- listM d value'
    return $ V.fromList ls

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
    let hsFunc = return x :: IO a
    let callStr = "(f())"
    (Just direct_ref) <- fromJSValue <$> runChakra (chakraEval evalStr)
    (Just direct_call) <- fromJSValue <$> runChakra (injectChakra hsFunc [] "f" >> chakraEval callStr)
    return $ and $ fmap (== x) [direct_ref, direct_call]

typePromiseRefl :: forall a. CanGenRefl a => Proxy a -> Property IO
typePromiseRefl _ = forAll $ \(x :: a) -> monadic $ do
    reportVar <- newIORef Nothing
    let hsFunc = MkJsPromise $ return x :: JsPromise a
    let reportFunc = writeIORef reportVar . Just :: a -> IO ()
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
    f :: Int -> Int -> String -> IO Int
    f a b s = return $ a + b + length s

canCatchBadCC :: Assertion
canCatchBadCC = do
  death <- newIORef False
  (runChakra $ injectChakra func [] "f" >> chakraEval "f()") `catchAny`
    \(_ :: SomeException) -> writeIORef death True >> return undefined
  readIORef death >>= (@=?) True
  where
    func a b = return $ a + b :: IO Integer
