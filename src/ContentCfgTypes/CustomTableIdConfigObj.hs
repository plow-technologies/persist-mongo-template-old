{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module ContentCfgTypes.CustomTableIdConfigObj where

import           Control.Applicative  ((<$>))
import           Prelude              hiding (head, init, last, readFile, tail,
                                       writeFile)

import           ContentCfgTypes.Util
import           Data.Text
import           Yesod

data CustomTableIdConfigObj =  CustomTableIdConfigObj {
     cTableId :: PersistValue
    }
   deriving (Read, Show,Eq)


instance FromJSON CustomTableIdConfigObj where
    parseJSON (Object cObj) = CustomTableIdConfigObj <$>
                          cObj .: "cTableId"
    parseJSON _ = fail "Rule: Expecting Table ConfigId Object Received, Other"

instance ToJSON CustomTableIdConfigObj where
    toJSON (CustomTableIdConfigObj {..}) = object
                        [
                         "cTableId"  .= cTableId
                         ]


-- | A Custom Table object transformer on a get parameter string

runCustomTableIdConfigObj :: (Text,Text) -> (Text, Value)
runCustomTableIdConfigObj (t,v)
  | t == "cTableId"  = (t .= textVal v)
  | otherwise = (t .= toJSON v)

defaultCTO :: CustomTableIdConfigObj
defaultCTO = undefined
