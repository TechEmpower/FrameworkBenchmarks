{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Bazel.Runfiles as Runfiles
import Control.Exception (handle, throwIO)
import Control.Monad.IO.Class.Linear (MonadIO)
import qualified Control.Functor.Linear as Linear
import Data.Aeson
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import DbHandler (createDbHandler)
import qualified Foreign.JNI
import Foreign.JNI.Safe (newGlobalRef_, withJVM, withLocalFrame_)
import Language.Java.Inline.Safe
import Language.Java.Safe (UnsafeUnrestrictedReference(..), reflect)
import System.Environment (getArgs)
import System.IO (stderr)
import qualified System.IO.Linear as Linear
import Wizzardo.Http.Handler (JHandler, createHandler)
import qualified Prelude
import Prelude (IO, (<>), map, ($))
import Prelude.Linear (Ur(..))

imports "com.wizzardo.http.*"
imports "com.wizzardo.http.framework.*"
imports "com.wizzardo.http.request.*"

main :: IO ()
main = do
    r <- Runfiles.create
    let jarPath = Runfiles.rlocation r "io_tweag_inline_java/wizzardo-http-benchmark/jar_deploy.jar"
        cpArg = "-Djava.class.path=" <> fromString jarPath
    args <- getArgs
    let -- We use the classpath provided at build time.
        otherJVMArgs =
          [ "-Xmx2G"
          , "-Xms2G"
          -- , "-server"
          , "-XX:+UseNUMA"
          , "-XX:+UseParallelGC"
          , "-XX:+AggressiveOpts"
          ]
    withJVM (cpArg : otherJVMArgs) $ showJVMExceptions $ withLocalFrame_ $ Linear.do
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
  where
    showJVMExceptions = handle $ \e ->
      Foreign.JNI.showException e Prelude.>>= Text.hPutStrLn stderr Prelude.>> throwIO e

createJsonHandler :: MonadIO m => m JHandler
createJsonHandler = createHandler $ \_req resp -> Linear.withLinearIO $ Linear.do
    jmsg <- reflect (toStrict $ encode $ jsonObject resp)
    [java| { $resp
            .setBody($jmsg)
            .appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
           } |]
    Linear.return (Ur ())
  where
    -- Don't inline, so the serialization is not cached.
    {-# NOINLINE jsonObject #-}
    jsonObject _ = object ["message" .= Text.pack "Hello, World!"]

createPlainTextHandler :: MonadIO m => m JHandler
createPlainTextHandler = Linear.do
    jmsg <- reflect (ByteString.Char8.pack "Hello, World!")
    UnsafeUnrestrictedReference jGlobalMsg <- newGlobalRef_ jmsg
    createHandler $ \_req resp -> Linear.withLinearIO $ Linear.do
      let ujmsg = UnsafeUnrestrictedReference jGlobalMsg
      [java| { $resp
               .setBody($ujmsg)
               .appendHeader(Header.KV_CONTENT_TYPE_TEXT_PLAIN);
             } |]
      Linear.return (Ur ())
