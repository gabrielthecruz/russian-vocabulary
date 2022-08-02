{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Plural where

import Import

getPluralR :: Handler Html
getPluralR = do
  defaultLayout $ do
    setTitle "Множественное Число"
    $(widgetFile "plural")
