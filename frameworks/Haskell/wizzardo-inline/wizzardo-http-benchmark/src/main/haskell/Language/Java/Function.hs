{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.Java.Function
  ( createBiFunction
  , createIntIntToObjFunction
  ) where

import Control.Exception (SomeException, catch)
import qualified Control.Monad
import qualified Control.Monad.IO.Class.Linear as Linear
import qualified Control.Monad.Linear.Builder as Linear
import qualified Control.Monad.Linear as Linear
import Data.Int
import Data.Singletons
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Foreign.JNI as JNI
import Foreign.JNI.Safe
import qualified Foreign.JNI.Types as NonLinear
import Foreign.Ptr
import GHC.Stable
import Language.Java.Inline.Safe
import Language.Java.Safe
import Prelude
import Prelude.Linear (Unrestricted(..))
import System.IO.Unsafe (unsafePerformIO)

imports "io.tweag.inline_java.wizzardo_http_benchmark.*"

type JNIFun f a
    = NonLinear.JNIEnv -> Ptr NonLinear.JObject -> StablePtrHandle f -> a

type JNIApplyFun
    = JNIFun
    (NonLinear.JObject -> NonLinear.JObject -> IO NonLinear.JObject)
    ( Ptr NonLinear.JObject
      -> Ptr NonLinear.JObject
      -> IO (Ptr NonLinear.JObject)
    )

type JNIIntIntToObjFun
    = JNIFun
        (Int32 -> Int32 -> IO NonLinear.JObject)
        (Int32 -> Int32 -> IO (Ptr NonLinear.JObject))

-- | A representation of a StablePtr that we can pass to Java
newtype StablePtrHandle a = StablePtrHandle Int64
  deriving Coercible

foreign import ccall "wrapper" wrapObjectFun
  :: JNIApplyFun -> IO (FunPtr JNIApplyFun)

foreign import ccall "wrapper" wrapIntIntToObjFun
  :: JNIIntIntToObjFun -> IO (FunPtr JNIIntIntToObjFun)

-- Export only to get a FunPtr.
foreign export ccall "wizzardo_http_handler_freeCallbackHandle" freeCallbackHandle
  :: NonLinear.JNIEnv -> Ptr JObject -> StablePtrHandle a -> IO ()
foreign import ccall "&wizzardo_http_handler_freeCallbackHandle" freeCallbackHandlePtr
  :: FunPtr (NonLinear.JNIEnv -> Ptr JObject -> StablePtrHandle a -> IO ())

freeCallbackHandle :: NonLinear.JNIEnv -> Ptr JObject -> StablePtrHandle a -> IO ()
freeCallbackHandle _ _ = freeStablePtr . handleToStablePtr

-- | Creates a BiFunction from a Haskell function.
--
-- The Haskell function must return jnull or a local reference.
--
-- TODO Maybe move this to a package to deal with function callbacks.
createBiFunction
  :: ( IsReferenceType a
     , IsReferenceType b
     , IsReferenceType c
     , SingI a
     , SingI b
     , SingI c
     , Linear.MonadIO m
     )
  => (NonLinear.J a -> NonLinear.J b -> IO (NonLinear.J c))
  -> m (J ('Class "java.util.function.BiFunction" <> [a, b, c]))
createBiFunction f =
    createCallback f registerNativesForBiFunction $ \longFunctionPtr ->
      unsafeGeneric Linear.<$>
      [java| new java.util.function.BiFunction() {
          @Override
          public Object apply(Object t, Object u) {
            return hsApply($longFunctionPtr, t, u);
          }

          private native void hsFinalize(long functionPtr);
          private native Object hsApply(long functionPtr, Object t, Object u);

          @Override
          public void finalize() { hsFinalize($longFunctionPtr); }
        } |]

-- Keep this function at the top level to ensure that no callback-specific state
-- leaks into the functions to register as native methods for all the instances
-- of the inner class.
registerNativesForBiFunction :: NonLinear.JClass -> IO ()
registerNativesForBiFunction = do
    let {-# NOINLINE applyPtr #-}
        applyPtr :: FunPtr JNIApplyFun
        applyPtr = unsafePerformIO $ wrapObjectFun $ \_jenv _jthis h reqPtr respPtr ->
          withJNICallbackHandle h nullPtr $ \handleFun ->
            NonLinear.unsafeObjectToPtr <$> Control.Monad.join
              (handleFun
                <$> NonLinear.objectFromPtr reqPtr
                <*> NonLinear.objectFromPtr respPtr
              )
    registerNativesForCallback $ JNI.JNINativeMethod
          "hsApply"
          (methodSignature
            [ SomeSing (sing :: Sing ('Prim "long"))
            , SomeSing (sing :: Sing ('Class "java.lang.Object"))
            , SomeSing (sing :: Sing ('Class "java.lang.Object"))
            ]
            (sing :: Sing ('Class "java.lang.Object"))
          )
          applyPtr


-- | Creates an object with a method @Object apply(int, int)@ that
-- invokes the given callback.
--
-- The Haskell callback must return jnull or a local reference.
--
createIntIntToObjFunction
  :: ( IsReferenceType a
     , SingI a
     , Linear.MonadIO m
     )
  => (Int32 -> Int32 -> IO (NonLinear.J a))
  -> m (J ('Iface "io.tweag.inline_java.wizzardo_http_benchmark.IntIntToObjFunction" <> '[a]))
createIntIntToObjFunction f =
    createCallback f registerNativesForIntIntToObjFunction $ \longFunctionPtr ->
      unsafeGeneric Linear.<$>
      [java| new IntIntToObjFunction() {
          @Override
          public Object apply(int t, int u) {
            return hsApply($longFunctionPtr, t, u);
          }

          private native void hsFinalize(long functionPtr);
          private native Object hsApply(long functionPtr, int t, int u);

          @Override
          public void finalize() { hsFinalize($longFunctionPtr); }
        } |]

-- Keep this function at the top level to ensure that no callback-specific state
-- leaks into the functions to register as native methods for all the instances
-- of the inner class.
registerNativesForIntIntToObjFunction :: NonLinear.JClass -> IO ()
registerNativesForIntIntToObjFunction = do
    let {-# NOINLINE applyPtr #-}
        applyPtr :: FunPtr JNIIntIntToObjFun
        applyPtr = unsafePerformIO $ wrapIntIntToObjFun $ \_jenv _jthis h t u ->
          withJNICallbackHandle h nullPtr $ \handleFun ->
            NonLinear.unsafeObjectToPtr <$> handleFun t u
    registerNativesForCallback $ JNI.JNINativeMethod
          "hsApply"
          (methodSignature
            [ SomeSing (sing :: Sing ('Prim "long"))
            , SomeSing (sing :: Sing ('Prim "int"))
            , SomeSing (sing :: Sing ('Prim "int"))
            ]
            (sing :: Sing ('Class "java.lang.Object"))
          )
          applyPtr

-- | Creates a Java function object that invokes the given Haskell
-- callback.
createCallback
  :: Linear.MonadIO m
  => f                                -- ^ Haskell callback
  -> (NonLinear.JClass -> IO ())      -- ^ Registers native methods for the Java
                                      -- class of the callback
  -> (StablePtrHandle f -> m (J ty))  -- ^ Instantiates the java callback which
                                      -- may have unregistered native methods
  -> m (J ty)
createCallback f registerNativesForCallback createJFunction =
    let Linear.Builder{..} = Linear.monadBuilder in do
    Unrestricted longFunctionPtr <- Linear.liftIOU (createStablePtrHandle f)
    jFunction <- createJFunction longFunctionPtr
    (jFunction, Unrestricted klass) <- getObjectClass jFunction
    Linear.liftIO (registerNativesForCallback klass)
    Linear.liftIO (JNI.deleteLocalRef klass)
    return jFunction

-- | Runs the Haskell callback referred by a 'StablePtrHandle' in the
-- context of a Java function.
--
-- It forwards Haskell exceptions to Java if any occur.
withJNICallbackHandle :: StablePtrHandle f -> a -> (f -> IO a) -> IO a
withJNICallbackHandle h valueOnException m =
    (derefStablePtrHandle h >>= m) `catch` \(e :: SomeException) ->
    fmap (const valueOnException) $ withLocalFrame_ $
    let Linear.Builder{..} = Linear.monadBuilder in do
    jmsg <- reflect (Text.pack $ show e)
    e <- [java| new RuntimeException($jmsg) |]
    throw_ (e :: J ('Class "java.lang.RuntimeException"))
  where
    derefStablePtrHandle :: StablePtrHandle a -> IO a
    derefStablePtrHandle = deRefStablePtr . handleToStablePtr

createStablePtrHandle :: a -> IO (StablePtrHandle a)
createStablePtrHandle a =
    StablePtrHandle . fromIntegral . ptrToIntPtr . castStablePtrToPtr <$>
    newStablePtr a

handleToStablePtr :: StablePtrHandle a -> StablePtr a
handleToStablePtr (StablePtrHandle h) =
    castPtrToStablePtr $ intPtrToPtr $ fromIntegral h

registerNativesForCallback :: JNI.JNINativeMethod -> NonLinear.JClass -> IO ()
registerNativesForCallback jniNativeMethod klass = do
    JNI.registerNatives klass
      [ jniNativeMethod
      , JNI.JNINativeMethod
          "hsFinalize"
          (methodSignature [SomeSing (sing :: Sing ('Prim "long"))] (sing :: Sing 'Void))
          freeCallbackHandlePtr
      ]
