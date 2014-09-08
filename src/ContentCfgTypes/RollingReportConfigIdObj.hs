{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module ContentCfgTypes.RollingReportConfigIdObj where

import           Prelude              hiding (head, init, last, readFile, tail,
                                       writeFile)

import           Control.Applicative  ((<$>))
import           Yesod

import           ContentCfgTypes.Util
import           Data.Text


-- TankGauge stuf

data RollingReportConfigIdObj =  RollingReportConfigIdObj {
  rollingReportConfigObjId :: PersistValue
} deriving (Read, Show,Eq)

instance FromJSON RollingReportConfigIdObj where
    parseJSON (Object tObj) = RollingReportConfigIdObj <$>
                          tObj .: "id"
    parseJSON _ = fail "Rule: Expecting RollingReportConfigIdObj received other"


instance ToJSON RollingReportConfigIdObj where
    toJSON (RollingReportConfigIdObj {..}) = object
                        [
                         "id" .= rollingReportConfigObjId
                         ]

runRollingReportConfigIdObj :: (Text,Text) -> (Text, Value)
runRollingReportConfigIdObj (t,v)
  | t == "id"  = (t .= textVal v)
  | otherwise = (t .= toJSON v)
