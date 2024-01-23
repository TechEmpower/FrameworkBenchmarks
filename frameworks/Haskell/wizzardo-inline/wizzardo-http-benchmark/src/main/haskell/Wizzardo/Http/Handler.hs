{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Wizzardo.Http.Handler
  ( JHandler
  , JRequest
  , JResponse
  , createHandler
  ) where

import qualified Control.Functor.Linear as Linear
import qualified Control.Monad
import qualified Control.Monad.IO.Class.Linear as Linear
import qualified Foreign.JNI.Types as NonLinear
import Language.Java.Function (createBiFunction)
import Language.Java.Inline.Safe
import Language.Java.Safe
import Prelude

imports "com.wizzardo.http.*"
imports "com.wizzardo.http.request.*"
imports "com.wizzardo.http.response.*"

type JHandler = J ('Class "com.wizzardo.http.Handler")
type JResponse = NonLinear.J Response
type JRequest = NonLinear.J Request
type Response = 'Class "com.wizzardo.http.response.Response"
type Request = 'Class "com.wizzardo.http.request.Request"

createHandler
  :: Linear.MonadIO m
  => (  UnsafeUnrestrictedReference JRequest
     -> UnsafeUnrestrictedReference JResponse
     -> IO ()
     )
  -> m JHandler
createHandler handle = Linear.do
    f <- createBiFunction $ \req resp ->
      handle
        (UnsafeUnrestrictedReference req)
        (UnsafeUnrestrictedReference resp)
      Control.Monad.>>
        Control.Monad.return resp
    [java| new Handler() {
          @Override
          public Response handle(Request req, Response resp) {
            return $f.apply(req, resp);
          }
      } |]
