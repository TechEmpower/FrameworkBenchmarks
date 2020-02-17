{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- The code in Java here has been copied from the benchmark wizzardo-http
-- in
-- https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/frameworks/wizzardo-http
module DbHandler (createDbHandler) where

import Control.Monad.IO.Class.Linear (MonadIO)
import qualified Control.Monad.Linear.Builder as Linear
import Data.Aeson (ToJSON(..), encode, object, (.=))
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int32)
import Data.String (fromString)
import Foreign.JNI.Safe (newGlobalRef_)
import qualified Language.Java as NonLinear
import Language.Java.Inline.Safe
import Language.Java.Function (createIntIntToObjFunction)
import Language.Java.Safe (J, JType(..), type (<>))
import Wizzardo.Http.Handler (JHandler, createHandler)
import Prelude (IO, Show, fromInteger, ($))
import Prelude.Linear (Unrestricted(..))
import qualified System.IO.Linear as Linear

imports "java.util.concurrent.ThreadLocalRandom"
imports "com.wizzardo.epoll.*"
imports "com.wizzardo.http.*"
imports "com.wizzardo.http.framework.*"
imports "com.wizzardo.http.request.*"
imports "com.wizzardo.http.response.*"
imports "io.reactiverse.pgclient.*"
imports "io.reactiverse.pgclient.impl.*"


createDbHandler :: MonadIO m => m JHandler
createDbHandler =
    let Linear.Builder{..} = Linear.monadBuilder in do
    encodeDbResponse <- createIntIntToObjFunction encodeDbResponseAsJSON
    Unrestricted jGlobalEncodeDbResponse <- newGlobalRef_ encodeDbResponse
    byteBufferProviderThreadLocal <- createThreadLocalByteBufferProvider
    Unrestricted jGlobalByteBufferProviderThreadLocal <-
      newGlobalRef_ byteBufferProviderThreadLocal
    poolRef <- createPgPoolRef
    Unrestricted jGlobalPoolRef <- newGlobalRef_ poolRef
    createHandler $ \req resp -> Linear.withLinearIO $ do
      let uPoolRef = Unrestricted jGlobalPoolRef
          uByteBufferProviderThreadLocal = Unrestricted jGlobalByteBufferProviderThreadLocal
          uEncodeDbResponse = Unrestricted jGlobalEncodeDbResponse
      [java| {
        int genWorldId = 1 + ThreadLocalRandom.current().nextInt(10000);
        $resp.async();
        $uPoolRef.get().preparedQuery("SELECT * FROM World WHERE id=$1", Tuple.of(genWorldId), dbRes -> {
            if (dbRes.succeeded()) {
                PgIterator resultSet = dbRes.result().iterator();
                if (!resultSet.hasNext()) {
                    $resp.status(Status._404);
                } else {
                    Tuple row = resultSet.next();
                    $resp.appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
                    $resp.setBody($uEncodeDbResponse.apply(row.getInteger(0), row.getInteger(1)));
                }
            } else {
                dbRes.cause().printStackTrace();
                $resp.status(Status._500).body(dbRes.cause().getMessage());
            }
            // commit async response
            ByteBufferProvider bufferProvider = $uByteBufferProviderThreadLocal.get();
            HttpConnection connection = $req.connection();
            $resp.commit(connection, bufferProvider);
            connection.flush(bufferProvider);
            $resp.reset();
        });
       } |]
      return (Unrestricted ())

data World = World { worldId :: Int32, worldRandomNumber :: Int32 }
  deriving Show

instance ToJSON World where
  toJSON w = object ["id" .= worldId w, "randomNumber" .= worldRandomNumber w]

createThreadLocalByteBufferProvider
  :: MonadIO m
  => m (J ('Class "java.lang.ThreadLocal" <>
            '[ 'Iface "com.wizzardo.epoll.ByteBufferProvider"]
          )
       )
createThreadLocalByteBufferProvider =
  [java| new ThreadLocal<ByteBufferProvider>() {
     @Override
     public ByteBufferProvider initialValue() {
       ByteBufferWrapper wrapper = new ByteBufferWrapper(64 * 1024);
       return () -> wrapper;
     }
   } |]

createPgPoolRef
 :: MonadIO m
 => m (J ('Class "java.lang.ThreadLocal" <> '[ 'Class "io.reactiverse.pgclient.PgPool"]))
createPgPoolRef =
  [java|
    new ThreadLocal() {
      @Override
      public PgPool initialValue() {
        WizzardoPgPoolOptions options = new WizzardoPgPoolOptions();
        options.setDatabase("hello_world");
        options.setHost("tfb-database");
        options.setPort(5432);
        options.setUser("benchmarkdbuser");
        options.setPassword("benchmarkdbpass");
        options.setCachePreparedStatements(true);
        options.setMaxSize(1);
        return new WizzardoPgPool(options);
      }
    }
   |]

encodeDbResponseAsJSON
  :: Int32 -> Int32 -> IO (NonLinear.J ('Array ('Prim "byte")))
encodeDbResponseAsJSON rowId rowRandomInt =
  NonLinear.reflect $ toStrict $ encode $ World rowId rowRandomInt
