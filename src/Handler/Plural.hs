{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Plural where

import Import

getPluralR :: Handler Html
getPluralR = do
    defaultLayout [whamlet|<h1 .header>Plural|]
