{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module ContentCfgTypes.StatusConfigObj where

import           Prelude              hiding (head, init, last, readFile, tail,
                                       writeFile)

import           Control.Applicative  ((<$>), (<*>), (<|>))
import           Yesod

import           ContentCfgTypes.Util
import           Data.Text

import qualified Control.Lens         as L
import qualified Data.Aeson.Lens      as L
import           Data.Maybe           (fromMaybe)


data StatusConfigObj =  StatusConfigObj {
      statusWidth            :: Int
      ,statusTrueText        :: Text
      ,statusFalseText       :: Text
      ,statusPidList         :: Text
      ,statusDescriptionList :: Text
      ,statusLocationList    :: Text
      ,statusSetValues       :: Text

    }
   deriving (Read, Show,Eq)

instance FromJSON StatusConfigObj where
    parseJSON v@ (Object tObj) = parseNormal <|> parseFallback
      where
        parseFallback = return $ fallbackSTCOParser v
        parseNormal   = StatusConfigObj <$>
                                    tObj .: "width"  <*>
                                    tObj .: "trueText"  <*>
                                    tObj .: "falseText"  <*>
                                    tObj .: "pidList" <*>
                                    tObj .: "descriptionList" <*>
                                    tObj .: "locationList" <*>
                                    tObj .: "setValues"


    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON StatusConfigObj where
    toJSON (StatusConfigObj {..}) = object
                        [
                         "width" .= statusWidth
                         ,"trueText" .= statusTrueText
                         ,"falseText" .= statusFalseText
                         ,"pidList" .= statusPidList
                         ,"descriptionList" .= statusDescriptionList
                         ,"locationList" .= statusLocationList
                         ,"setValues" .= statusSetValues
                         ]


fallbackSTCOParser :: L.AsValue s => s -> StatusConfigObj
fallbackSTCOParser v = StatusConfigObj {
     statusWidth          = fromMaybe statusWidth w
     ,statusTrueText      = fromMaybe statusTrueText tt
     ,statusFalseText     = fromMaybe statusFalseText ft
     ,statusPidList       = fromMaybe statusPidList pl
     ,statusDescriptionList = fromMaybe statusDescriptionList dl
     ,statusLocationList    = fromMaybe statusLocationList ll
     ,statusSetValues       = fromMaybe statusSetValues sv
    }

  where
    (StatusConfigObj {..} ) = defaultStCO
    w  = v L.^? (L.members . L.key "width"          . L._Integral )
    tt = v L.^? (L.members . L.key "trueText"         .  L._String)
    ft = v L.^? (L.members . L.key "falseText"       .  L._String)
    pl = v L.^? (L.members . L.key "pidList"          .  L._String)
    dl = v L.^? (L.members . L.key "descriptionList"         .  L._String)
    ll = v L.^? (L.members . L.key "locationList"     .  L._String)
    sv = v L.^? (L.members . L.key "setValues"     .  L._String)



-- | A Status object transformer on a get parameter string

runStatusConfigObj :: (Text,Text) -> (Text, Value)
runStatusConfigObj (t,v)
  | t == "width" = (t .= intVal v)
  | t == "trueText" = (t .= textVal v)
  | t == "falseText" = (t .= textVal v)
  | t == "pidList" = (t .= textVal v)
  | t == "descriptionList" = (t .= textVal v)
  | t == "locationList" = (t .= textVal v)
  | t == "setValues" = (t .= textVal v)
  | otherwise = (t .= toJSON v)


defaultStCO :: StatusConfigObj
defaultStCO = StatusConfigObj 200 "" "" "" "" "" ""
