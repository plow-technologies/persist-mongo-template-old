{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module ContentCfgTypes.TankGaugeConfigObj where

import           Prelude              hiding (head, init, last, readFile, tail,
                                       writeFile)

import           Control.Applicative  ((<$>), (<*>), (<|>))
import           Yesod

import           ContentCfgTypes.Util
import           Data.Text

import qualified Control.Lens         as L
import qualified Data.Aeson.Lens      as L
import           Data.Maybe           (fromMaybe)

-- TankGauge stuf

data TankGaugeConfigObj =  TankGaugeConfigObj {
      tankGaugeWidth        :: Int
     ,tankGaugeHeight       :: Int
     ,tankGaugeStacking     :: Text
     ,tankGaugeUnits        :: Text
     ,tankGaugeColors       :: Text
     ,tankGaugeLineColors   :: Text
     ,tankGaugeLineParams   :: Text
     ,tankGaugeWacLevels    :: Text
     ,tankGaugeDescriptions :: Text
     ,tankGaugeLocations    :: Text
     ,tankGaugeParamIds     :: Text
    }
   deriving (Read, Show,Eq)

instance FromJSON TankGaugeConfigObj where
    parseJSON v@ (Object tObj) = parseNormal <|> (parseFallback)
       where
          parseFallback = return $ fallbackTGCOParser v
          parseNormal = TankGaugeConfigObj <$>
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

instance ToJSON TankGaugeConfigObj where
    toJSON (TankGaugeConfigObj {..}) = object
                        [
                         "width" .= tankGaugeWidth
                         ,"height" .= tankGaugeHeight
                         ,"stacking" .= tankGaugeStacking
                         ,"units" .= tankGaugeUnits
                         ,"colors" .= tankGaugeColors
                         ,"linecolors" .= tankGaugeLineColors
                         ,"lineparams" .= tankGaugeLineParams
                         ,"waclevels" .= tankGaugeWacLevels
                         ,"descriptionlist" .= tankGaugeDescriptions
                         ,"locationlist" .= tankGaugeLocations
                         ,"pidlist" .= tankGaugeParamIds
                         ]

defaultTGCO :: TankGaugeConfigObj
defaultTGCO = TankGaugeConfigObj 600 400 "normal" "" "" "" "" "" "" "" ""


fallbackTGCOParser :: L.AsValue s => s -> TankGaugeConfigObj
fallbackTGCOParser v = TankGaugeConfigObj {
      tankGaugeWidth        =  fromMaybe tankGaugeWidth         w
     ,tankGaugeHeight       =  fromMaybe tankGaugeHeight        h
     ,tankGaugeStacking     =  fromMaybe tankGaugeStacking      s
     ,tankGaugeUnits        =  fromMaybe tankGaugeUnits         u
     ,tankGaugeColors       =  fromMaybe tankGaugeColors        c
     ,tankGaugeLineColors   =  fromMaybe tankGaugeLineColors    lc
     ,tankGaugeLineParams   =  fromMaybe tankGaugeLineParams    lp
     ,tankGaugeWacLevels    =  fromMaybe tankGaugeWacLevels     wl
     ,tankGaugeDescriptions =  fromMaybe tankGaugeDescriptions  dl
     ,tankGaugeLocations    =  fromMaybe tankGaugeLocations     ll
     ,tankGaugeParamIds     =  fromMaybe tankGaugeParamIds      pl
    }

  where
    (TankGaugeConfigObj {..} ) = defaultTGCO
    w  = v L.^? (L.members . L.key "width"          . L._Integral )
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


-- | A TankGauge object transformer on a get parameter string

runTankGaugeConfigObj :: (Text,Text) -> (Text, Value)
runTankGaugeConfigObj (t,v)
  | t == "width" = (t .=  intVal v)
  | t == "height" = (t .=  intVal v)
  | t == "stacking" = (t .=  textVal v)
  | t == "units" = (t .=  textVal v)
  | t == "colors" = (t .=  textVal v)
  | t == "linecolors" = (t .=  textVal v)
  | t == "lineparams" = (t .=  textVal v)
  | t == "waclevels" = (t .=  textVal v)
  | t == "descriptionlist" = (t .=  textVal v)
  | t == "locationlist" = (t .=  textVal v)
  | t == "pidlist" = (t .=  textVal v)
  | otherwise = (t .= toJSON v)
