{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Persist.Mongo.Settings ( module Plowtech.Persist.Settings
                              , runDB
                              , readDBConf
                              ) where

import           Data.Aeson                 ((.!=), (.:?))
import           Control.Applicative        ((<$>), (<*>))
import           Data.ByteString            hiding (group, unpack)
import           Data.Text                  (Text, unpack)
import           Data.Time
import           Database.Persist.MongoDB
import           Database.Persist.Quasi     (lowerCaseSettings)
import           GHC.Generics
import           Language.Haskell.TH.Syntax hiding (location)
import           Network                    (PortID (PortNumber))
import           Persist.Mongo.Lens
import           Yesod                      hiding (runDB)
import qualified Data.ByteString            as BS
import qualified Data.Yaml                  as Y


import           ContentCfgTypes
import           Data.Typeable
import           Permissions
import           WidgetTypes
import           Plowtech.Persist.Settings

-- share [mkPersist (mkPersistSettings (ConT ''MongoBackend)) { mpsGeneric = False }, mkMigrate "migrateAll"][persistLowerCase|
-- Questionnaire
--   desc Text Maybe
--   questions [Question]
--   deriving Show Eq Read
-- Question
--   formulation Text
--   deriving Show Eq Read
-- |]

instance ToJSON a => ToJSON (Entity a) where
  toJSON = keyValueEntityToJSON

instance FromJSON a => FromJSON (Entity a) where
  parseJSON = keyValueEntityFromJSON

-- JSON
instance ToJSON OnpingTagCombined where
    toJSON (OnpingTagCombined {..}) = object
                                      ["location_id"          .= onpingTagCombinedLocation_id            ,
                                       "slave_parameter_id"  .= onpingTagCombinedSlave_parameter_id      ,
                                       "parameter_tag_id"    .= onpingTagCombinedParameter_tag_id        ,
                                       "description"         .= onpingTagCombinedDescription             ,
                                       "unit_id"             .= onpingTagCombinedUnit_id                 ,
                                       "status_active"       .= onpingTagCombinedStatus_active           ,
                                       "status_writable"     .= onpingTagCombinedStatus_writable         ,
                                       "last_update"         .= onpingTagCombinedLast_update             ,
                                       "result"              .= onpingTagCombinedResult                  ,
                                       "validation_code"     .= onpingTagCombinedValidation_code         ,
                                       "permissions"         .= onpingTagCombinedPermissions             ,
                                       "delete"              .= onpingTagCombinedDelete                  ,
                                       "company"	     .= onpingTagCombinedCompanyIdRef		 ,
                                       "site"	             .= onpingTagCombinedSiteIdRef      	 ,
                                       "location"	     .= onpingTagCombinedLocation		 ,
                                       "pid"                 .= onpingTagCombinedPid
                                      ]

instance FromJSON OnpingTagCombined where
    parseJSON (Object o) = OnpingTagCombined <$>
                                      o .:? "location_id"         .!= Nothing <*>
                                      o .:? "slave_parameter_id"  .!= Nothing <*>
                                      o .:? "parameter_tag_id"    .!= Nothing <*>
                                      o .:? "description"         .!= Nothing <*>
                                      o .:? "unit_id"             .!= Nothing <*>
                                      o .:? "status_active"       .!= Nothing <*>
                                      o .:? "status_writable"     .!= Nothing <*>
                                      o .:? "last_update"         .!= Nothing <*>
                                      o .:? "result"              .!= Nothing <*>
                                      o .:? "validation_code"     .!= Nothing <*>
                                      o .:? "permissions"         .!= Nothing <*>
                                      o .:? "delete"              .!= Nothing <*>
                                      o .:? "company"	          .!= Nothing <*>
                                      o .:? "site"	          .!= Nothing <*>
                                      o .:? "location"	          .!= Nothing <*>
                                      o .:? "pid"                 .!= Nothing
    parseJSON _ = fail "Rule: Expecting OnpingTagCombined Recieved Other"


instance ToJSON Location where
    toJSON (Location {..}) = object
                             [  "site"	   .=  locationSiteIdRef
                              , "slaveId"  .=  locationSlaveId
                              , "refId"    .=  locationRefId
                              , "name"	   .=  locationName
                              , "url"	   .=  locationUrl
                              , "delete"   .=  locationDelete
                              , "company"  .=  locationCompanyIdRef
                             ]

instance FromJSON Location where
    parseJSON (Object l) = Location <$>
                           l .: "site"	     <*>
                           l .: "slaveId"   <*>
                           l .: "refId"     <*>
                           l .: "name"	     <*>
                           l .: "url"	     <*>
                           l .: "delete"    <*>
                           l .: "company"
    parseJSON _ = fail "Rule: Expecting Object {site:<val>,slaveId:<val>,refId:<val>,name:<val>,url:<val>,delete:<val>,company:<val>} recieved other}"



{-===========================================================================-}
{-                                 runDB                                     -}
{-===========================================================================-}

runDB :: forall (m :: * -> *) b.(MonadIO m ,MonadBaseControl IO m) =>
               Action m b -> m b
runDB a = withMongoDBConn "onping_production" "10.84.207.130" (PortNumber 27017) Nothing 2000 $ \pool -> do
  (runMongoDBPool master a )  pool

readDBConf :: FilePath -> IO (Either String MongoDBConf)
readDBConf fPath = do
	fCont <- BS.readFile fPath
	return $ Y.decodeEither $ fCont



instance ToJSON Group where
instance FromJSON Group where

instance ToJSON UserTag where
instance FromJSON UserTag where

instance ToJSON RollingReportConfig where
instance FromJSON RollingReportConfig where

instance ToJSON RollingReportConfigEntry where
instance FromJSON RollingReportConfigEntry  where

instance ToJSON RollingReportPid where
instance FromJSON RollingReportPid  where

instance ToJSON OnpingTagHistory where
    toJSON (OnpingTagHistory {..}) = object
                                      [
                                        "pid"	              .= onpingTagHistoryPid
                                      , "time"	              .= onpingTagHistoryTime
                                      , "val"	              .= onpingTagHistoryVal
                                      ]


{-|
data Entity entity =
    Entity { entityKey :: Key entity
           , entityVal :: entity }
    deriving (Eq, Ord, Show, Read)
|-}

-- | lensEntity :: Lens (Entity a) (Entity b) a b
lensEntityVal :: Functor f => (a -> f a) -> Entity a -> f (Entity a)
lensEntityVal  f (Entity k v) = fmap (Entity k) (f v)

persistMakeClassy ''SplineConfigObj

persistMakeClassy ''ContentConfig

persistMakeClassy ''ContentObj

persistMakeClassy ''ContentArray

persistMakeClassy ''MenuPanel

persistMakeClassy ''Dashboard

persistMakeClassy ''SubObject
persistMakeClassy ''SubMenuJoin
persistMakeClassy ''ContentArrayJoin
persistMakeClassy ''OnpingAlarmCombined
persistMakeClassy ''OnpingTagCombined
persistMakeClassy ''OnpingTagHistory
persistMakeClassy ''ParameterHistory
persistMakeClassy ''TestCollection
persistMakeClassy ''Company

persistMakeClassy ''Site

persistMakeClassy ''Location
persistMakeClassy ''Robot

persistMakeClassy ''Unit
persistMakeClassy ''LocationTableWidget
persistMakeClassy ''MultiLocationTableWidget
persistMakeClassy ''PDFTableWidget
persistMakeClassy ''CalendarWidget
persistMakeClassy ''Prospective
persistMakeClassy ''ReportRow
persistMakeClassy ''AutoReport
persistMakeClassy ''MaskScript
persistMakeClassy ''MaskTypeJoin
persistMakeClassy ''MaskDataStore
persistMakeClassy ''MaskType
persistMakeClassy ''TableByMultiLocConfigObj
persistMakeClassy ''TableByLocConfigObj
persistMakeClassy ''CustomTableConfigObj
persistMakeClassy ''CustomTableIdConfigObj
persistMakeClassy ''RollingReportConfigIdObj
