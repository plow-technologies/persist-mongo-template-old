{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module ContentCfgTypes.AngularGaugeConfigObj where

import           Prelude              hiding (head, init, last, readFile, tail,
                                       writeFile)

import           Control.Applicative  ((<$>), (<*>), (<|>))
import           Yesod

import           ContentCfgTypes.Util
import           Data.Text

import           Data.Maybe

import qualified Control.Lens         as L
import qualified Data.Aeson.Lens      as L

-- AngularGauge stuf

data AngularGaugeConfigObj =  AngularGaugeConfigObj {
      angularGaugeWidth        :: Int
     ,angularGaugeHeight       :: Int
     ,angularGaugeStacking     :: Text
     ,angularGaugeUnits        :: Text
     ,angularGaugeColors       :: Text
     ,angularGaugeLineColors   :: Text
     ,angularGaugeLineParams   :: Text
     ,angularGaugeWacLevels    :: Text
     ,angularGaugeDescriptions :: Text
     ,angularGaugeLocations    :: Text
     ,angularGaugeParamIds     :: Text
    }
   deriving (Read, Show,Eq)

instance FromJSON AngularGaugeConfigObj where
    parseJSON v@(Object tObj) = parserNormal <|> (parserFallback)
     where
      parserFallback  = return $ fallbackAGCOParser v
      parserNormal = AngularGaugeConfigObj <$>
                          tObj .: "width" <*>
                          tObj .: "height" <*>
                          tObj .: "stacking" <*>
                          tObj .: "units" <*>
                          tObj .: "colors" <*>
                          tObj .: "linecolors" <*>
                          tObj .: "lineparams" <*>
                          tObj .: "waclevels" <*>
                          tObj .: "descriptionlist" <*>
                          tObj .: "locationlist" <*>
                          tObj .: "pidlist"

    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON AngularGaugeConfigObj where
    toJSON (AngularGaugeConfigObj {..}) = object
                        [
                         "width" .= angularGaugeWidth
                         ,"height" .= angularGaugeHeight
                         ,"stacking" .= angularGaugeStacking
                         ,"units" .= angularGaugeUnits
                         ,"colors" .= angularGaugeColors
                         ,"linecolors" .= angularGaugeLineColors
                         ,"lineparams" .= angularGaugeLineParams
                         ,"waclevels" .= angularGaugeWacLevels
                         ,"descriptionlist" .= angularGaugeDescriptions
                         ,"locationlist" .= angularGaugeLocations
                         ,"pidlist" .= angularGaugeParamIds
                         ]

defaultAGCO :: AngularGaugeConfigObj
defaultAGCO = AngularGaugeConfigObj 250 250 "normal" "" "" "" "" "" "" "" ""

fallbackAGCOParser :: L.AsValue s => s -> AngularGaugeConfigObj
fallbackAGCOParser v = AngularGaugeConfigObj {
      angularGaugeWidth        =  fromMaybe angularGaugeWidth         w
     ,angularGaugeHeight       =  fromMaybe angularGaugeHeight        h
     ,angularGaugeStacking     =  fromMaybe angularGaugeStacking      s
     ,angularGaugeUnits        =  fromMaybe angularGaugeUnits         u
     ,angularGaugeColors       =  fromMaybe angularGaugeColors        c
     ,angularGaugeLineColors   =  fromMaybe angularGaugeLineColors    lc
     ,angularGaugeLineParams   =  fromMaybe angularGaugeLineParams    lp
     ,angularGaugeWacLevels    =  fromMaybe angularGaugeWacLevels     wl
     ,angularGaugeDescriptions =  fromMaybe angularGaugeDescriptions  dl
     ,angularGaugeLocations    =  fromMaybe angularGaugeLocations     ll
     ,angularGaugeParamIds     =  fromMaybe angularGaugeParamIds      pl
    }

  where
    (AngularGaugeConfigObj {..} ) = defaultAGCO
    w  = v L.^? (L.members . L.key "width" . L._Integral )
    h  = v L.^? (L.members . L.key "height"         .  L._Integral)
    s  = v L.^? (L.members . L.key "stacking"       .  L._String)
    u  = v L.^? (L.members . L.key "units"          .  L._String)
    c  = v L.^? (L.members . L.key "colors"         .  L._String)
    lc = v L.^? (L.members . L.key "linecolors"     .  L._String)
    lp = v L.^? (L.members . L.key "lineparams"     .  L._String)
    wl = v L.^? (L.members . L.key "waclevels"      .  L._String)
    dl = v L.^? (L.members . L.key "descriptionlist".  L._String)
    ll = v L.^? (L.members . L.key "locationlist"   .  L._String)
    pl = v L.^? (L.members . L.key "pidlist"        .  L._String)


-- | A AngularGauge object transformer on a get parameter string

runAngularGaugeConfigObj :: (Text,Text) -> (Text, Value)
runAngularGaugeConfigObj (t,v)
  | t == "width" = (t .=  intVal v)
  | t == "height" = (t .=  intVal v)
  | t == "units" = (t .=  textVal v)
  | t == "stacking" = (t .=  textVal v)
  | t == "colors" = (t .=  textVal v)
  | t == "linecolors" = (t .=  textVal v)
  | t == "lineparams" = (t .=  textVal v)
  | t == "waclevels" = (t .=  textVal v)
  | t == "descriptionlist" = (t .=  textVal v)
  | t == "locationlist" = (t .=  textVal v)
  | t == "pidlist" = (t .=  textVal v)
  | otherwise = (t .= toJSON v)

