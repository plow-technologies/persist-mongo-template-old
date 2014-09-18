{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module ContentCfgTypes.Util where
import           Prelude          hiding (head, init, last, map, readFile, tail,
                                   writeFile, zipWith)


import           Data.Aeson.Types
import           System.Locale
import qualified WidgetTypes      as W
import           Yesod
-- Types
import           Data.Text
import           Data.Time
import           Text.Read

-- | Helper functions to explicity do type conversion

intVal :: Text -> Value
intVal = toJSON.intRead
  where
    intRead :: Text -> Int
    intRead = read.unpack

intMaybeVal :: Text -> Value
intMaybeVal = toJSON . intMaybeParse
 where
  intMaybeParse :: Text -> Maybe Int
  intMaybeParse = readMaybe.unpack

intListVal :: Text -> Value
intListVal = toJSON.intListRead
  where
    intListRead :: Text -> [Int]
    intListRead = read.unpack

boolVal :: Text -> Value
boolVal = toJSON.boolRead
  where
    boolRead :: Text -> Bool
    boolRead = read.unpack

textVal :: Text -> Value
textVal = toJSON

utcVal :: Text -> Value
utcVal = toJSON.timeRead
    where
      timeRead :: Text -> UTCTime
      timeRead = (readTime defaultTimeLocale (unpack W.stdTimeFormat)).unpack

-- | A Table object transformer on a get parameter string
-- | "o51ffc5907671f95ad8000000"
locVal :: Text -> Value
locVal = toJSON . cnvServe.cnv
    where
        cnv :: Text -> Value
        cnv = toJSON.unpack
        cnvServe :: (Value -> Maybe PersistValue)
        cnvServe t = case fromJSON t of
                       (Success l) -> Just l
                       (Error _ ) -> Nothing


text2PersistVal :: Text -> Value
text2PersistVal = toJSON . cnvServe.cnv
    where
        cnv :: Text -> Value
        cnv = toJSON.unpack
        cnvServe :: (Value -> Maybe PersistValue)
        cnvServe t = case fromJSON t of
                       (Success l) -> Just l
                       (Error _ ) -> Nothing


