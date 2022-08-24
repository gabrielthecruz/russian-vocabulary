{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Plural where

import Data.List.Split (chunksOf)
import Import
import Utils (accentedWidget, shuffle)

getPluralR :: Handler Html
getPluralR = do
  plurals <- runDB $ selectList [WordFormWordFormType ==. "ru_noun_pl_nom"] []
  shuffledWords <- liftIO $ shuffle 100 plurals
  ruWords <- runDB $ selectList [RuWordWordType ==. Just "noun", RuWordId <-. map (wordFormWordId . entityVal) shuffledWords] []
  let pluralWords =
        chunksOf 20 $
          take 40 $
            [ (ruWord, ruForm)
              | ruWord@(Entity wordKey _) <- ruWords,
                ruForm@(Entity _ ruFormItem) <- plurals,
                wordKey == wordFormWordId ruFormItem
            ]
  defaultLayout $ do
    setTitle "Plurals - Русский Словарь"
    $(widgetFile "plural")
