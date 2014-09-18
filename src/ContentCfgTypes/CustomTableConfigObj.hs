{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module ContentCfgTypes.CustomTableConfigObj where

import           ContentCfgTypes.Util
import           Control.Applicative  ((<$>), (<*>), (<|>))
import           Data.Text
import           GHC.Generics
import           Prelude              hiding (head, init, last, readFile, tail,
                                       writeFile)
import           Yesod

import qualified Control.Lens         as L
import qualified Data.Aeson.Lens      as L
import qualified Data.List            as LST
import           Data.Maybe           (fromMaybe)

data CTCell =  CPid CellPid
              | CText Text
      deriving (Read, Show, Generic, Eq)
instance FromJSON CTCell where
instance ToJSON CTCell where


data CellPid = CellPid { cPid :: Int }
    deriving(Read, Show, Eq)
instance FromJSON CellPid where
  parseJSON (Object cObj) = CellPid <$>
                        cObj .: "pid"
  parseJSON _ = fail "Rule: Expecting CellPid object received other"

instance ToJSON CellPid where
  toJSON (CellPid {..}) = object
              [
                "pid" .= cPid
              ]

data CustomTableConfigObj =  CustomTableConfigObj {
     cTableTitle    :: Text
     ,cTableHeaders :: [Text]
     ,cTableCells   :: [[CTCell]]
    }
   deriving (Read, Show,Eq)


instance FromJSON CustomTableConfigObj where
    parseJSON v@ (Object cObj) = parseNormal <|> (parseFallback)
      where
            parseFallback = return $ fallbackCTOParser v
            parseNormal = CustomTableConfigObj <$>
                                          cObj .: "title"  <*>
                                          cObj .: "headers" <*>
                                          cObj .: "cells"
    parseJSON _ = fail "Rule: Expecting Table Config Object Received, Other"

instance ToJSON CustomTableConfigObj where
    toJSON (CustomTableConfigObj {..}) = object
                        [
                         "title"  .= cTableTitle
                         ,"headers" .= cTableHeaders
                         ,"cells" .= cTableCells
                         ]

fallbackCTOParser :: L.AsValue s => s -> CustomTableConfigObj
fallbackCTOParser v = CustomTableConfigObj {
       cTableTitle        =  fromMaybe cTableTitle         t
     , cTableHeaders      =  if (LST.null cTableHeaders) then h else cTableHeaders
     , cTableCells     =  if (LST.null cTableCells) then defaultCells else cTableCells

  }

  where
    (CustomTableConfigObj {..} ) = defaultCTAO
    t  = v L.^? (L.members . L.key "title"          . L._String )
    h  = v L.^.. (L.members . L.key "headers"        . L.values. L._String)



-- | A Custom Table object transformer on a get parameter string

runCustomTableConfigObj :: (Text,Text) -> (Text, Value)
runCustomTableConfigObj (t,v)
  | t == "title"  = (t .= textVal v)
  | t == "headers" = (t .=  textVal v)
  | t == "cells" = (t .= textVal v)
  | otherwise = (t .= toJSON v)

defaultCTAO :: CustomTableConfigObj
defaultCTAO = CustomTableConfigObj "Title" ["Column 1", "Column 2","Column 3","Column 4"] defaultCells

defaultCells :: [[CTCell]]
defaultCells = [[CText "Cell 1,1",CText "Cell 1,2",CText "Cell 1,3"],[CPid (CellPid 299),CPid (CellPid 299),CPid (CellPid 300)],[CText "Cell 3,1",CText "Cell 3,2",CText "Cell 3,3"],[CText "Cell 4,1",CText "Cell 4,2",CText "Cell 4,3"]]
