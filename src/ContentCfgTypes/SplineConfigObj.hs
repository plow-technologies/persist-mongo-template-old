{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances,FlexibleContexts  #-}
module ContentCfgTypes.SplineConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)
import Control.Applicative ((<$>), (<*>))
import Yesod
import Data.Text
-- import Data.Maybe
import Data.Functor.Identity
import ContentCfgTypes.Util
import Debug.Trace
-- import Text.Read
import Text.Parsec
-- import Text.Parsec.Prim
-- import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as P 

data SplineConfigObj =  SplineConfigObj { 
     splineStep :: Int
     ,splineTitle :: Text
     ,splineParamIds :: Text
     ,splineTime :: Int
     ,splineTimeUnit :: Text
     ,splineEndDate :: Text
     ,splineLegend :: Int
     ,splineDescriptionList :: Text
     ,splineLocationList :: Text
     ,splineGraphList :: Text
     ,splineSecondYAxisList :: Text
    }
   deriving (Show,Eq)
-- withRemaining :: Parser a -> Parser (a, String)

withRemaining :: Monad m =>
                       ParsecT a u m a1 -> ParsecT a u m (a1, a)
withRemaining p = (,) <$> p <*> getInput


parsecToReadsPrec :: Stream s Identity t1 =>
                           ParsecT s () Identity a -> t -> s -> [(a, s)]

parsecToReadsPrec parsecParser _ input
    = case parse (withRemaining $ parsecParser) "" input of
        Left s -> traceShow s []
        Right result -> [result]


instance Read SplineConfigObj where
    readsPrec d r = splineConfigObjReads d r

splineConfigObjReads :: t ->  String -> [(SplineConfigObj, String)]
splineConfigObjReads = parsecToReadsPrec lexSplineConfigObj --parserSplineConfigObj 
                       



-- readSplineConfigObj = case parse parserSplineConfigObj "" of 

def :: GenLanguageDef String st Identity
def = emptyDef { P.reservedNames = [
                  "splineStep"
                 ,"splineTitle"
                 ,"splineParamIds"
                 ,"splineTime"
                 ,"splineTimeUnit"
                 ,"splineEndDate"
                 ,"splineLegend"
                 ,"splineDescriptionList"
                 ,"splineLocationList"
                 ,"splineGraphList"
                 ,"splineSecondYAxisList"
                 ,"SplineConfigObj"
                 ]
               ,P.reservedOpNames = ["="]}




spline_lexer :: P.GenTokenParser String u Identity 
spline_lexer = P.makeTokenParser def 


lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = P.lexeme spline_lexer

reserved :: String -> ParsecT String u Identity ()
reserved = P.reserved spline_lexer

reservedOp :: String -> ParsecT String u Identity ()
reservedOp = P.reservedOp spline_lexer

lParens :: ParsecT String u Identity a -> ParsecT String u Identity a 
lParens = P.parens spline_lexer

integer :: ParsecT String u Identity Int
integer = P.integer spline_lexer >>= return.fromIntegral


text :: ParsecT String u Identity Text
text = (P.stringLiteral spline_lexer) >>= return.pack 


braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = (P.braces spline_lexer) 


comma ::  ParsecT String u Identity String
comma = P.comma spline_lexer


parseSplineStep :: ParsecT String u Identity Int
parseSplineStep = do  { reserved "splineStep";   reservedOp "=" ;  integer }

parserSplineTitle :: ParsecT String u Identity Text
parserSplineTitle           =     do { reserved  "splineTitle"           ;  reservedOp  "="; text }

parserSplineParamIds :: ParsecT String u Identity Text
parserSplineParamIds        =     do { reserved  "splineParamIds"        ;  reservedOp  "="; text }

parserSplineTime :: ParsecT String u Identity Int
parserSplineTime            =     do { reserved  "splineTime"            ;  reservedOp  "="; integer }


parserSplineTimeUnit :: ParsecT String u Identity Text
parserSplineTimeUnit        =     do { reserved  "splineTimeUnit"        ;  reservedOp  "="; text }

parserSplineEndDate :: ParsecT String u Identity Text
parserSplineEndDate         =     do { reserved  "splineEndDate"         ;  reservedOp  "="; text }

parserSplineLegend  :: ParsecT String u Identity Int
parserSplineLegend          =     do { reserved  "splineLegend"          ;  reservedOp  "="; integer  }

parserSplineDescriptionList :: ParsecT String u Identity Text
parserSplineDescriptionList =     do { reserved  "splineDescriptionList" ;  reservedOp  "="; text }

parserSplineLocationList :: ParsecT String u Identity Text
parserSplineLocationList    =     do { reserved  "splineLocationList"    ;  reservedOp  "="; text }

parserSplineGraphList :: ParsecT String u Identity Text
parserSplineGraphList       =     do { reserved  "splineGraphList"       ;  reservedOp  "="; text }

parserSplineSecondYAxisList :: ParsecT String u Identity Text
parserSplineSecondYAxisList = do { 
                                _ <- comma;     
                                reserved  "splineSecondYAxisList" ;  reservedOp  "="; text }
                              <|> return ""



lexSplineConfigObj :: ParsecT String u Identity SplineConfigObj
lexSplineConfigObj = spaces>>parenthesizedSplineConfigObj



parenthesizedSplineConfigObj :: ParsecT String u Identity SplineConfigObj
parenthesizedSplineConfigObj = do {lParens parenthesizedSplineConfigObj;}
                               <|> parserSplineConfigObj

parserSplineConfigObj :: ParsecT String u Identity SplineConfigObj
parserSplineConfigObj = do  { 
                          reserved "SplineConfigObj" ; 
                          braces innerSplineObj;
                            }
    where innerSplineObj = do 
            v0 <- parseSplineStep;                 _ <-  comma;                              
            v1 <- parserSplineTitle;               _ <-  comma;                   
            v2 <- parserSplineParamIds;            _ <-  comma;                   
            v3 <- parserSplineTime;                _ <-  comma;                   
            v4 <- parserSplineTimeUnit;            _ <-  comma;                   
            v5 <- parserSplineEndDate;             _ <-  comma;                   
            v6 <- parserSplineLegend;              _ <-  comma;                   
            v7 <- parserSplineDescriptionList;     _ <-  comma;                   
            v8 <- parserSplineLocationList;        _ <-  comma;                   
            v9 <- parserSplineGraphList;                         
            v10 <- parserSplineSecondYAxisList;                                    
            return $ SplineConfigObj v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 (v10)

  

localParseTest :: String
localParseTest = "  (((SplineConfigObj {splineStep = 600, splineTitle = \"Enter Title Here\", splineParamIds = \"299,300,\", splineTime = 3, splineTimeUnit = \"hour\", splineEndDate = \"\", splineLegend = 1, splineDescriptionList = \"Pufin Well -- 2 - Modif Channel 1 Reading ,Pufin Well -- 3 - Modif Channel 2 Reading ,\", splineLocationList = \"6,6,\", splineGraphList = \"line,line,\"})))"
testLocalParse :: SplineConfigObj
testLocalParse = read localParseTest


-- readSplineConfig :: ParsecT String () IO Int
-- readSplineConfig = string "SplineConfigObj" >> valueParser 
--     where 
--       spaceString s = spaces >> string s >> spaces
--       spaceEq = spaces >> char '=' >> spaces
--       myString = stringLiteral
--       fieldValue s = spaceString s >> spaceEq 
--       valueParser = spaces >> do { char '{';
--                                    step <- spaceString "splineStep" >> spaceEq >> many digit >>= return.read;
--                                    title <- char ','>> spaceString "splineTitle" >> spaceEq >> myString;
--                                    return step;
--                                  };
                                   
  

instance FromJSON SplineConfigObj where 
    parseJSON (Object tObj) = SplineConfigObj <$>  
                          tObj .: "step"  <*>
                          tObj .: "title" <*>
                          tObj .: "pidlist" <*>
                          tObj .: "time" <*>
                          tObj .: "timeUnit" <*>
                          tObj .: "endDate" <*>
                          tObj .: "legend" <*>
                          tObj .: "descriptionList" <*>
                          tObj .: "locationList" <*>
                          tObj .: "graphList"     <*>
                          tObj .: "secondYAxisList"


    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON SplineConfigObj where 
    toJSON (SplineConfigObj {..}) = object 
                        [ 
                         "step"  .= splineStep
                         ,"title" .= splineTitle
                         ,"pidlist" .= splineParamIds
                         ,"time" .= splineTime
                         ,"timeUnit" .= splineTimeUnit
                         ,"endDate" .= splineEndDate
                         ,"legend" .= splineLegend
                         ,"descriptionList" .= splineDescriptionList
                         ,"locationList" .= splineLocationList
                         ,"graphList" .= splineGraphList
                         ,"secondYAxisList" .= splineSecondYAxisList
                         ]


-- | A Spline object transformer on a get parameter string

runSplineConfigObj :: (Text,Text) -> (Text, Value)
runSplineConfigObj (t,v) 
  | t == "step"  = (t .= intVal v)
  | t == "title" = (t .=  textVal v)
  | t == "pidlist" = (t .= textVal v)
  | t == "time" = (t .= intVal v)
  | t == "timeUnit" = (t .= textVal v)
  | t == "endDate" = (t .= textVal v)
  | t == "legend" = (t .= intVal v)
  | t == "descriptionList" = (t .= textVal v)
  | t == "locationList" = (t .= textVal v)
  | t == "graphList" = (t .= textVal v)
  | t == "secondYAxisList" = (t .= textVal v)
  | otherwise = (t .= toJSON v)

defaultSCO :: SplineConfigObj
defaultSCO = SplineConfigObj 600 "Enter Title Here" "" 3 "hour" "" 1 "" "" "" ""
