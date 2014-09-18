{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module ContentCfgTypes.TankConfigObj where

import           Prelude              hiding (head, init, last, readFile, tail,
                                       writeFile)

import           Control.Applicative  ((<$>), (<*>))
import           Yesod

import           ContentCfgTypes.Util
import           Data.Text

data TankConfigObj =  TankConfigObj {
      tankWidth :: Int
     ,tankTitle :: Text
    }
   deriving (Read, Show,Eq)

instance FromJSON TankConfigObj where
    parseJSON (Object tObj) = TankConfigObj <$>
                          tObj .: "width" <*>
                          tObj .: "title"

    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON TankConfigObj where
    toJSON (TankConfigObj {..}) = object
                        [
                         "width" .= tankWidth
                         ,"title" .= tankTitle
                         ]

-- | A Tank object transformer on a get parameter string
runTankConfigObj :: (Text,Text) -> (Text, Value)
runTankConfigObj (t,v)
  | t == "width" = (t .= intVal v)
  | t == "title" = (t .=  textVal v)
  | otherwise = (t .= toJSON v)

