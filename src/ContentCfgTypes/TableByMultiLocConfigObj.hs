{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module ContentCfgTypes.TableByMultiLocConfigObj where

import           ContentCfgTypes.Util
import           Control.Applicative  ((<$>))
import           Data.Text
import           Prelude              hiding (head, init, last, readFile, tail,
                                       writeFile)
import           Yesod

emptyTableByMultiLocConfigObj :: TableByMultiLocConfigObj
emptyTableByMultiLocConfigObj = TableByMultiLocConfigObj (toPersistValue k)
   where k :: Text
         k = ""
-- Table stuff


data TableByMultiLocConfigObj = TableByMultiLocConfigObj {
     mlcoTableId :: PersistValue
    }
   deriving (Read, Show,Eq)

instance FromJSON TableByMultiLocConfigObj where
    parseJSON (Object tObj) = TableByMultiLocConfigObj <$>
                          tObj .: "tableid"
    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON TableByMultiLocConfigObj where
    toJSON (TableByMultiLocConfigObj {..}) = object
                                             [ "tableid" .= mlcoTableId   ]



runTableByMultiLocConfigObj :: (Text,Text) -> (Text, Value)
runTableByMultiLocConfigObj (t,v)
  | t == "tableid" = (t .=  locVal v)
  | otherwise = (t .= toJSON v)
