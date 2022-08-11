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

getRows :: Int -> [a] -> [[a]]
getRows _ [] = [[]]
getRows n x = take n x : getRows n (drop n x)

getPluralR :: Handler Html
getPluralR = do
  ruWords <- runDB $ selectList [RuWordWordType <-. [Just "noun"]] []
  ruForms <- runDB $ selectList [WordFormWordId <-. map entityKey (take 25 ruWords), WordFormWordFormType ==. "ru_noun_pl_nom"] []
  let wordsWithPlural =
        [ (ruWordAccented ruWord, wordFormForm wordForm, toPathPiece ruWordKey, wordFormBare wordForm)
          | Entity ruWordKey ruWord <- ruWords,
            Entity _ wordForm <- ruForms,
            ruWordKey == wordFormWordId wordForm
        ]
  defaultLayout $ do
    setTitle "Plurals - Русский Словарь"
    $(widgetFile "plural")
