{-# LANGUAGE OverloadedStrings #-}

module Views.Fortune
    ( renderFortunes
    ) where

import           Control.Monad    (forM_)
import           Text.Blaze.Html5 as H

import           Models.Fortune


renderFortunes :: [Fortune] -> Html
renderFortunes fs =
    docTypeHtml $ do
        H.head $
            H.title "Fortunes"
        H.body $
            H.table $ do
                H.tr $ do
                    H.th "id"
                    H.th "message"
                forM_ fs $ \f ->
                    H.tr $ do
                        H.td $ toHtml $ _idF f
                        H.td $ toHtml $ _messageF f
