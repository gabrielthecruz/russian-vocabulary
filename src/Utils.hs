{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Utils where

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

accentedWidget :: Bool -> String -> Widget
accentedWidget useBold word =
  let accented = parseAccented word
   in $(widgetFile "accented-word")