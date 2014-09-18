{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module ContentCfgTypes.MultiParameterHistoryReportConfigObj where
import           Data.Typeable                                  (Typeable)
import           GHC.Generics
import           Prelude                                        hiding (head,
                                                                 init, last,
                                                                 readFile, tail,
                                                                 writeFile)

import           Control.Applicative                            ((<$>), (<*>))
import           Yesod


import           Data.Time

import           ContentCfgTypes.MultiParameterHistoryConfigObj
import           ContentCfgTypes.Util
import           Data.Text

-- | ==================================================
-- | MultiParameterHistoryReportConfigObj config
-- | ==================================================

data MultiParameterHistoryReportConfigObj =  MultiParameterHistoryReportConfigObj {
      rhistoryStep    :: Double
     ,rhistoryDelta   :: Double
     ,rhistoryStart   :: UTCTime
     ,rhistoryEnd     :: UTCTime
     ,rhistoryPIDList :: Text
     ,rhistoryTitle   :: Text
    }
   deriving (Read, Show, Eq,Generic,Typeable)

instance FromJSON MultiParameterHistoryReportConfigObj where
    parseJSON (Object tObj) = MultiParameterHistoryReportConfigObj <$>
                          tObj .: "step"  <*>
                          tObj .: "dt" <*>
                          tObj .: "startDate" <*>
                          tObj .: "endDate"   <*>
                          tObj .: "pidList"   <*>
                          tObj .: "title"


    parseJSON _ = fail "Rule: Expecting multi parameter history object object received, other"

instance ToJSON MultiParameterHistoryReportConfigObj where
    toJSON (MultiParameterHistoryReportConfigObj {..}) = object
                        [
                         "step"  .= rhistoryStep
                         ,"dt" .= rhistoryDelta
                         ,"startDate" .= rhistoryStart
                         ,"endDate"   .= rhistoryEnd
                         ,"pidList" .=   rhistoryPIDList
                         ,"title" .= rhistoryTitle

                         ]

reportToConfigObj :: MultiParameterHistoryReportConfigObj -> MultiParameterHistoryConfigObj
reportToConfigObj (MultiParameterHistoryReportConfigObj step delta start end pidList _) = (MultiParameterHistoryConfigObj step delta start end pidList)

-- | A Tank object transformer on a get parameter string

runMultiParameterHistoryReportConfigObj :: (Text,Text) -> (Text, Value)
runMultiParameterHistoryReportConfigObj (t,v)
  | t == "step"  = (t .= intVal v)
  | t == "dt" = (t .= intVal v)
  | t == "startDate" = (t .= utcVal v)
  | t == "endDate"   = (t .= utcVal v)
  | t == "pidList"   = (t .= textVal v)
  | t == "title"     = (t .= textVal v)
  | otherwise    = (t .= toJSON v)

