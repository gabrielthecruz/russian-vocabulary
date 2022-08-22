{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Plural where

import Data.List (nub)
import Import
import System.CPUTime (getCPUTime)
import System.Random (Random (randomRs), mkStdGen)
import qualified Prelude

rndIndex :: Int -> IO [Int]
rndIndex len = do
  cpuTime <- getCPUTime
  let gen = mkStdGen $ fromIntegral cpuTime
  return $ nub $ randomRs (0, len) gen

shuffle :: Int -> [a] -> IO [a]
shuffle n x = do
  indexes <- liftIO $ rndIndex $ length x
  return $ take n [x Prelude.!! i | i <- indexes]

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
  shuffledWords <- liftIO $ shuffle 100 ruWords
  ruForms <- runDB $ selectList [WordFormWordId <-. map entityKey shuffledWords, WordFormWordFormType ==. "ru_noun_pl_nom"] [LimitTo 40]
  let wordsWithPlural =
        [ (ruWordAccented ruWord, wordFormForm wordForm, toPathPiece ruWordKey, wordFormBare wordForm)
          | Entity ruWordKey ruWord <- ruWords,
            Entity _ wordForm <- ruForms,
            ruWordKey == wordFormWordId wordForm
        ]
      pluralWords1 = take 20 wordsWithPlural
      pluralWords2 = take 20 $ reverse wordsWithPlural
  defaultLayout $ do
    setTitle "Plurals - Русский Словарь"
    $(widgetFile "plural")
