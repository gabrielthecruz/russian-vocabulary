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
import Import hiding (words)
import Utils (accentedWidget, shuffle)

getPluralR :: Handler Html
getPluralR = do
  plurals <- runDB $ selectList [WordFormWordFormType ==. "ru_noun_pl_nom"] []
  shuffledWords <- liftIO $ shuffle 100 plurals
  let bannedKeys = [wordFormWordId word | Entity _ word <- shuffledWords, wordFormPosition word == Just 2]
      words = filter (not . (`elem` bannedKeys)) $ map (wordFormWordId . entityVal) shuffledWords
  ruWords <- runDB $ selectList [RuWordWordType ==. Just "noun", RuWordId <-. words] []
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
