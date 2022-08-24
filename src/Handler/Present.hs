{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Handler.Present where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Import hiding (intercalate, keys)
import Utils (accentedWidget, shuffle)

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
  verbs <- runDB $ selectList [RuWordWordType ==. Just "verb"] []
  shuffled <- liftIO $ shuffle 30 verbs
  wordsForms <- runDB $ selectList [WordFormWordId <-. map entityKey shuffled, WordFormWordFormType <-. presFut] []
  translations <- runDB $ selectList [RuWordTranslationWordId <-. map entityKey shuffled] []
  let ruWordsRows =
        chunksOf 2 $ -- rows
          take 10 $
            filter
              (\(_, forms, _) -> length forms == 6) -- filters only what has all pronouns for present tense
              [ ( ruWord,
                  filter (\(Entity _ wordForm) -> ruWordKey == wordFormWordId wordForm) wordsForms,
                  map (ruWordTranslationTranslation . entityVal) $
                    filter (\(Entity _ val) -> ruWordKey == ruWordTranslationWordId val) translations
                )
                | ruWord@(Entity ruWordKey ruWordItem) <- shuffled,
                  foldr ((||) . (`isSuffixOf` ruWordBare ruWordItem)) False ["ать", "еть", "ить"]
              ]
  defaultLayout $ do
    setTitle "Present Tense - Русский Словарь"
    $(widgetFile "verbs-present")