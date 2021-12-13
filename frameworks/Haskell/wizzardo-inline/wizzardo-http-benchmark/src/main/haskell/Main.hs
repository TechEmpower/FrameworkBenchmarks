{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Control.Monad
import Control.Monad.IO.Class.Linear (MonadIO)
import qualified Control.Monad.Linear.Builder as Linear
import Data.Aeson
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import qualified Data.Text as Text
import DbHandler (createDbHandler)
import Foreign.JNI.Safe (newGlobalRef_, withJVM, withLocalFrame_)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Java.Inline.Safe
import Language.Java.Safe (reflect)
import System.Environment (getArgs, lookupEnv)
import qualified System.IO.Linear as Linear
import Wizzardo.Http.Handler (JHandler, createHandler)
import Prelude (IO, (=<<), concat, fromInteger, map, ($), (++))
import Prelude.Linear (Unrestricted(..))
import Paths_wizzardo_http_benchmark (getDataFileName)

imports "com.wizzardo.http.*"
imports "com.wizzardo.http.framework.*"
imports "com.wizzardo.http.request.*"

main :: IO ()
main =
    getDataFileName "build/libs/wizzardo-http-benchmark.jar" Control.Monad.>>= \jar ->
    getArgs Control.Monad.>>= \args -> do
    let -- We use the classpath provided at build time.
        cp = concat $ jar : ":" :
               Maybe.maybeToList $(TH.lift =<< TH.runIO (lookupEnv "CLASSPATH"))
        jvmArgs = [ fromString ("-Djava.class.path=" ++ cp) ]
        otherJVMArgs =
          [ "-Xmx2G"
          , "-Xms2G"
          -- , "-server"
          , "-XX:+UseNUMA"
          , "-XX:+UseParallelGC"
          , "-XX:+AggressiveOpts"
          ]
    withJVM (jvmArgs ++ otherJVMArgs) $ withLocalFrame_ $
      let Linear.Builder{..} = Linear.monadBuilder in do
      jsonHandler <- createJsonHandler
      jPlainTextHandler <- createPlainTextHandler
      jDbHandler <- createDbHandler
      jargs <- reflect (map Text.pack args)
      [java| {
        WebApplication application = new WebApplication($jargs) {
            @Override
            protected void initHttpPartsCache() {
                ByteTree tree = httpStringsCache.getTree();
                for (Request.Method method : Request.Method.values()) {
                    tree.append(method.name());
                }
                tree.append(HttpConnection.HTTP_1_1);
            }
        };

        application.onSetup(app -> {
          app.getUrlMapping()
             .append("/json", $jsonHandler)
             .append("/plaintext", $jPlainTextHandler)
             .append("/db", $jDbHandler);
        });
        application.start();
       } |]

createJsonHandler :: MonadIO m => m JHandler
createJsonHandler = createHandler $ \_req resp -> Linear.withLinearIO $
    let Linear.Builder{..} = Linear.monadBuilder in do
    jmsg <- reflect (toStrict $ encode $ jsonObject resp)
    [java| { $resp
            .setBody($jmsg)
            .appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
           } |]
    return (Unrestricted ())
  where
    -- Don't inline, so the serialization is not cached.
    {-# NOINLINE jsonObject #-}
    jsonObject _ = object ["message" .= Text.pack "Hello, World!"]

createPlainTextHandler :: MonadIO m => m JHandler
createPlainTextHandler =
    let Linear.Builder{..} = Linear.monadBuilder in do
    jmsg <- reflect (ByteString.Char8.pack "Hello, World!")
    Unrestricted jGlobalMsg <- newGlobalRef_ jmsg
    createHandler $ \_req resp -> Linear.withLinearIO $ do
      let ujmsg = Unrestricted jGlobalMsg
      [java| { $resp
               .setBody($ujmsg)
               .appendHeader(Header.KV_CONTENT_TYPE_TEXT_PLAIN);
             } |]
      return (Unrestricted ())
