{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module contains one off boilerplate that is not usually required
module MIME (
    Plain
  , HTML
  , Json
) where

import qualified TFB.Types as Types
import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NE

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.BufferBuilder.Json    as Json
import qualified Html
import           Servant                    as S
import           Network.HTTP.Media         ((//), (/:))

-- * PlainText without charset
-- This is necessary to fulfill TFB requirements.
-- The built-in servant Plaintext MIME is richer with encoding
-- https://hackage.haskell.org/package/servant-0.15/docs/Servant-API.html#t:PlainText 

data Plain
instance S.Accept Plain where contentType _ = "text" // "plain"
instance S.MimeRender Plain ByteString where
  mimeRender _ = id
  {-# INLINE mimeRender #-}

-- * HTML
-- TODO: package the following block of code into a library akin to 'servant-lucid'

data HTML
instance S.Accept HTML where
    contentTypes _ =
      "text" // "html" /: ("charset", "utf-8") NE.:|
      ["text" // "html"]
instance Html.Document a => S.MimeRender HTML a where
  {-# SPECIALIZE S.mimeRender :: S.Proxy HTML -> Types.FortunesHtml -> ByteString #-}
  mimeRender _ = Html.renderByteString

-- * JSON
-- The built-in servant JSON mime only works with Aeson.
-- https://hackage.haskell.org/package/servant-0.15/docs/Servant-API.html#t:JSON
-- For performance we use BufferBuilder; hence we need to describe our own Mime.
-- TODO: package the following block of code into a library akin to 'servant-lucid'

data Json
instance S.Accept Json where
  contentTypes _ = "application" // "json" NE.:| []
instance Json.ToJson a => S.MimeRender Json a where
  {-# SPECIALIZE S.mimeRender :: S.Proxy Json -> Json.ObjectBuilder -> ByteString #-}
  {-# SPECIALIZE S.mimeRender :: S.Proxy Json -> Types.World -> ByteString #-}
  mimeRender _ = LBS.fromStrict . Json.encodeJson

instance S.FromHttpApiData Types.Count where
  parseQueryParam
    = pure . Types.mkCount . Either.fromRight 1 . parseQueryParam
