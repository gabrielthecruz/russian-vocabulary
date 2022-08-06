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

parseAccented :: String -> [(Bool, Char)]
parseAccented [] = []
parseAccented (x : '\'' : xs) = (True, x) : parseAccented xs
parseAccented (x : xs) = (False, x) : parseAccented xs

getPluralR :: Handler Html
getPluralR = do
  ruWords <- runDB $ selectList [RuWordLevel <-. [Just "A1", Just "A2", Nothing], RuWordWordType <-. [Just "noun"]] []
  ruForms <- runDB $ selectList [WordFormWordId <-. map entityKey (take 50 ruWords), WordFormWordFormType <-. ["ru_noun_pl_nom", "ru_adj_pl_nom"]] []
  let wordsWithPlural =
        [ (ruWordAccented ruWord, wordFormForm wordForm, toPathPiece ruWordKey)
          | Entity ruWordKey ruWord <- ruWords,
            Entity _ wordForm <- ruForms,
            ruWordKey == wordFormWordId wordForm
        ]
      wordsCount = length ruWords
  defaultLayout $ do
    setTitle "Plurals - Русский Словарь"
    $(widgetFile "plural")
