{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Present where

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

presFut :: [String]
presFut =
  [ "ru_verb_presfut_sg1",
    "ru_verb_presfut_sg2",
    "ru_verb_presfut_sg3",
    "ru_verb_presfut_pl1",
    "ru_verb_presfut_pl2",
    "ru_verb_presfut_pl3"
  ]

parseWordFormType :: String -> String
parseWordFormType x
  | x == "ru_verb_presfut_sg1" = "Я"
  | x == "ru_verb_presfut_sg2" = "Ты"
  | x == "ru_verb_presfut_sg3" = "Он · Она · Оно"
  | x == "ru_verb_presfut_pl1" = "Мы"
  | x == "ru_verb_presfut_pl2" = "Вы"
  | x == "ru_verb_presfut_pl3" = "Они"
  | otherwise = "?"

getPresentR :: Handler Html
getPresentR = do
  allRuWords <- runDB $ selectList [RuWordWordType <-. [Just "verb"]] []
  shuffledWords <- liftIO $ shuffle 20 allRuWords
  wordsForms <- runDB $ selectList [WordFormWordId <-. map entityKey shuffledWords, WordFormWordFormType <-. presFut] []
  let keys = nub [key | Entity key _ <- allRuWords, Entity _ wordForm <- wordsForms, key == wordFormWordId wordForm]
      ruWords = take 10 [Entity ruWordKey ruWord | key <- keys, Entity ruWordKey ruWord <- allRuWords, ruWordKey == key]
  ruWordsTranslation <- runDB $ selectList [RuWordTranslationWordId <-. keys] []
  defaultLayout $ do
    setTitle "Verbs in Present - Русский Словарь"
    $(widgetFile "verbs-present")
