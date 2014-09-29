{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.Persist
import           Persist.Mongo.Settings
import           Prelude                (IO, print, ($), (.))
import           Control.Applicative    ((<$>))
import           Data.Either

import           Plowtech.Persist.Settings


main :: IO ()
main = do
  res <- runDB $ selectList [] [Asc DashboardId]
  print $ dashboardDefault.entityVal <$> res
  mConf <- readDBConf "config.yml"
  case mConf of
  	(Left s) -> print s
  	(Right conf) -> do
  		newRes <- runDBConf conf $ selectList [] [Asc DashboardId]
  		print $ dashboardDefault . entityVal <$> newRes
