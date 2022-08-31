{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.SpecificCase where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Import hiding (intercalate, keys)
import Utils (accentedWidget, shuffle)

caseNameToFormType :: String -> [String]
caseNameToFormType name
  | name == "genitive" = ["ru_noun_sg_gen", "ru_noun_pl_gen"]
  | name == "prepositional" = ["ru_noun_sg_prep", "ru_noun_pl_prep"]
  | name == "accusative" = ["ru_noun_sg_acc", "ru_noun_pl_acc"]
  | name == "dative" = ["ru_noun_sg_dat", "ru_noun_pl_dat"]
  | name == "instrumental" = ["ru_noun_sg_inst", "ru_noun_pl_inst"]
  | otherwise = [""]

parseWordFormType :: String -> String
parseWordFormType x
  | x == "ru_noun_sg_gen" = "Genitive (sg.)"
  | x == "ru_noun_sg_dat" = "Dative (sg.)"
  | x == "ru_noun_sg_acc" = "Accusative (sg.)"
  | x == "ru_noun_sg_inst" = "Instrumental (sg.)"
  | x == "ru_noun_sg_prep" = "Prepositional (sg.)"
  | x == "ru_noun_pl_gen" = "Genitive (pl.)"
  | x == "ru_noun_pl_dat" = "Dative (pl.)"
  | x == "ru_noun_pl_acc" = "Accusative (pl.)"
  | x == "ru_noun_pl_inst" = "Instrumental (pl.)"
  | x == "ru_noun_pl_prep" = "Prepositional (pl.)"
  | otherwise = "?"

getPageName :: String -> String
getPageName name
  | name == "genitive" = "Родительный падеж"
  | name == "prepositional" = "Предложный падеж"
  | name == "accusative" = "Винительный падеж"
  | name == "dative" = "Дательный падеж"
  | name == "instrumental" = "Творительный падеж"
  | otherwise = "?"

getSpecificCaseR :: String -> Handler Html
getSpecificCaseR caseName = do
  wordsForms <- runDB $ selectList [WordFormWordFormType <-. caseNameToFormType caseName] []
  shuffled <- liftIO $ shuffle 20 wordsForms
  ruWords <- runDB $ selectList [RuWordWordType ==. Just "noun", RuWordId <-. map (wordFormWordId . entityVal) shuffled] []
  translations <- runDB $ selectList [RuWordTranslationWordId <-. map (wordFormWordId . entityVal) shuffled] []
  let ruWordsRows =
        chunksOf 2 $ -- rows
          take 8 $
            [ ( ruWord,
                filter (\(Entity _ wordForm) -> ruWordKey == wordFormWordId wordForm) wordsForms,
                map (ruWordTranslationTranslation . entityVal) $
                  filter (\(Entity _ val) -> ruWordKey == ruWordTranslationWordId val) translations
              )
              | ruWord@(Entity ruWordKey _) <- ruWords
            ]
  let pageName = getPageName $ toLower caseName
  defaultLayout $ do
    setTitle "Cases - Русский Словарь"
    $(widgetFile "verbs-present")
