{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Plural where

import Import

getPluralR :: Handler Html
getPluralR = do
  ruWords <- runDB $ selectList [RuWordLevel <-. [Just "A1", Just "A2", Nothing], RuWordWordType <-. [Just "noun", Just "adjective"]] []
  ruForms <- runDB $ selectList [WordFormWordId <-. (map entityKey $ take 150 ruWords), WordFormWordFormType <-. ["ru_noun_pl_nom", "ru_adj_pl_nom"]] []
  let wordsWithPlural =
        [ (ruWordAccented ruWord, wordFormForm wordForm, show $ ruWordWordType ruWord)
          | Entity ruWordKey ruWord <- ruWords,
            Entity _ wordForm <- ruForms,
            ruWordKey == wordFormWordId wordForm
        ]
      wordsCount = length ruWords
  defaultLayout $ do
    setTitle "Plurals - Русский Словарь"
    $(widgetFile "plural")
