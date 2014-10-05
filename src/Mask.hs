{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveGeneric,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Mask where 

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)

import Control.Monad
--import Control.Lens
-- import Control.Lens.At
import GHC.Generics
-- import qualified Data.Maybe as MB
import qualified Data.HashMap.Strict as M
-- import Language.StructuredScript
import Language.StructuredScript.Parsers 
import Mask.Types
import Data.SafeCopy
import Persist.Mongo.Settings
import Data.Serialize hiding (get)
import SafeCopy () 
import qualified Data.Aeson as A
import Text.Read (readMaybe )
import Control.Applicative hiding (Const)
-- import Import
import qualified Data.Set as S
import qualified Data.List as L
import Yesod hiding (runDB)
import qualified Data.Traversable as T
-- import Data.Traversable.Compat
import Data.Text hiding (zip)
import Data.Text.Read

import Plowtech.Persist.Settings (MongoDBConf, runDBConf)
import qualified Plowtech.Persist.Mask as PM
import Plowtech.Persist.Mask.Types


-- | Add your builtIn Functions here, BuiltIns and in Types
maskLookup :: MongoDBConf -> MaskData  -> IO MaskFcn
maskLookup = PM.maskLookup

maskPullOut :: MaskFcn -> Const -> Either String Const
maskPullOut (OneVar f ) = f
maskPullOut (UserDef f) = f

exampleUserMask :: Either String UserMask
exampleUserMask = UserMask <$>  sstStmtTree <*>  sstILookup
   where
     sstStmtTree = sstParse testString
     sstILookup =  Right sampleTagIMap

-- | triggerTransform allows even single Variable functions to be defined with DB paramters

triggerTransform :: MongoDBConf -> UserMask ->
                    (VarTable -> Const -> Either String Const) ->  IO ( Const -> Either String Const)
triggerTransform mdbc (UserMask _ tm) fcn = do
  vtReady <- M.traverseWithKey (keyTraverseFcn mdbc) tm
  return $ fcn (VT vtReady)
    
keyTraverseFcn :: MongoDBConf -> Text -> (TagTarget Int) -> IO Const
keyTraverseFcn = PM.keyTraverseFcn

tagMapTransform :: MongoDBConf -> UserMask -> IO (Const -> Either String Const)
tagMapTransform = PM.tagMapTransform

mkUserMask :: Stmt -> UserMask 
mkUserMask stmts = emptyUserMask { getStmtTree = stmts}

mkStmt :: SSTConfig -> Either String Stmt 
mkStmt (SSTConfig _ _ wc) = sstParse (unpack wc)

-- |pKey is always the first Int value of [Int]

makeMaskTypeIdFromJSON :: Text -> Maybe MaskTypeId
makeMaskTypeIdFromJSON = cnvServe.cnv
    where 
      cnv :: Text -> A.Value 
      cnv = A.toJSON.unpack
      cnvServe :: (Value -> Maybe MaskTypeId)
      cnvServe t = case A.fromJSON t of 
                     (A.Success l) -> Just l
                     (A.Error _) -> Nothing


data SSTConfig = SSTConfig{
                         inputValues :: ![Integer], -- Test values used in mask creation
                         inputNames  :: ![Text],    -- Names of various input labels in order
                         workingCode :: !Text --structured script code body
} deriving (Show, Eq, Generic)

instance A.ToJSON SSTConfig
instance A.FromJSON SSTConfig


testStructuredScript :: SSTConfig
testStructuredScript = SSTConfig [] [] ""


decodeMaskAssignConfig :: A.Value -> A.Result MaskAssignConfig
decodeMaskAssignConfig = A.fromJSON
     
maskAssignConfigToMaskTypeJoin :: MongoDBConf -> MaskAssignConfig -> IO (Either Text MaskTypeJoin) 
maskAssignConfigToMaskTypeJoin = PM.maskAssignConfigToMaskTypeJoin

makeBuiltInDataStore :: [Int] -> BuiltInId -> IO (Either Text MaskDataStore)
makeBuiltInDataStore i bid = return $ PM.makeBuiltInDataStore i bid

makeUserDefDataStore :: MongoDBConf -> [Int] -> MaskTypeId -> IO (Either Text MaskDataStore)
makeUserDefDataStore mdbc keys mtid = do  
  mmsktype <- runDBConf mdbc $ get mtid
  case mmsktype of 
    (Just msktype) -> do
                  let mskdDataStore = maskTypeValue msktype
                      edecodedMDS = maskDataDecode mskdDataStore
                      edecodeMaskData = userMask <$> edecodedMDS
                      edecodeBuiltInId = getBuiltInId <$> edecodedMDS
                      einsertedKeys = (flip insertPidKeys keys) <$> edecodeMaskData
                  return.eStringToEText $ mkMaskDataStore <$> edecodeBuiltInId <*> einsertedKeys
    Nothing -> return $ Left err
        where 
          err :: Text 
          err = "makeUserDefDataStore failed to encode a MaskDataStore"
  
resetMTJDefault :: MongoDBConf -> Int -> Bool -> IO ()
resetMTJDefault mdbc primaryKey dflt = do
  case dflt of
    True -> runDBConf mdbc $ updateWhere [(MaskTypeJoinPKey ==. primaryKey),(MaskTypeJoinDefaultSelect ==. True)] [MaskTypeJoinDefaultSelect =. False]
    False -> return ()

getMaskFunctionDefault :: MongoDBConf -> Int -> IO (Either Text (Const -> Either String Const)) 
getMaskFunctionDefault = PM.getMaskFunctionDefault


mdsToFcn :: MongoDBConf -> [Int] -> MaskDataStore -> IO (Either Text (Const -> Either String Const ))
mdsToFcn = PM.mdsToFcn

onpingTagCombinedDefaultTransform :: MongoDBConf -> OnpingTagCombined -> IO OnpingTagCombined
onpingTagCombinedDefaultTransform = PM.onpingTagCombinedDefaultTransform
    
returnDefaultMaskFunction :: MongoDBConf -> Int -> IO (Const -> Either String Const)   
returnDefaultMaskFunction mdbc pid = do
  etcesc <- getMaskFunctionDefault mdbc pid
  case etcesc of
    Left _ -> return (\ x -> Right $ x)  
    Right cescFcn -> return cescFcn  
    
textToConst :: Text -> Either String Const
textToConst txt = do
  let doubledtxt = double txt
  case doubledtxt of
    Left e -> Left e
    Right dbl -> return $ ConstDouble (fst dbl)  
    
constToText :: Const -> Text
constToText cr = let esot = constToEitherDouble cr
                 in case esot of
                   Left e -> pack.show $ e   
                   Right txt -> pack.show $ txt

constToEitherDouble :: Const -> Either String Double
constToEitherDouble (ConstDouble x) = Right x
constToEitherDouble (_) = Left "Expected a Double, but got something else"

-- | Takes a list of PIDs and a historical list
onpingTagHWrapper :: (Const -> Either String Const) -> OnpingTagHistory -> OnpingTagHistory
onpingTagHWrapper _ oth@(OnpingTagHistory _ _  Nothing ) = oth
onpingTagHWrapper cFcn oth@(OnpingTagHistory _ _  (Just v)) = oth {onpingTagHistoryVal = (etom $ (doubleToConst v) >>= cFcn >>= constToEitherDouble)}
  where
     etom (Left _) = Nothing  
     etom (Right x) = Just x

onpingTagHistoryDefaultTransform :: MongoDBConf -> [Int] -> [OnpingTagHistory] -> IO [OnpingTagHistory]
onpingTagHistoryDefaultTransform _ a b
  | (L.null a) || (L.null b) = return []
onpingTagHistoryDefaultTransform  mdbc pids oth = do
  let uniquePids = S.toList.S.fromList $ pids  
  uniqueMaskFcns <- extracted mdbc uniquePids
  let pairPandF = zip uniquePids uniqueMaskFcns
  return [ f o | o@(OnpingTagHistory ph _ vh) <- oth , (p , f) <- pairPandF , ph == (Just p), vh /= Nothing]

extracted :: T.Traversable f =>  MongoDBConf -> f Int -> IO (f (OnpingTagHistory -> OnpingTagHistory))
extracted = PM.extracted
            
doubleToConst :: Double -> Either String Const
doubleToConst dbl = do
  return $ ConstDouble dbl


-- |utility function

eStringToEText :: Either String a -> Either Text a
eStringToEText (Left x ) = Left (pack x) 
eStringToEText (Right x) = Right x


returnMaskedOTCResult :: MongoDBConf -> Alarm -> IO (Either Text OnpingTagCombined)
returnMaskedOTCResult mdbc alm = do
  let mmtid = alarmMaskId alm
      aPids@(pkey:_) = alarmPids alm
  mmds <- runDBConf mdbc $ myGet mmtid  
  aMDS <- case mmds of
    Nothing -> return $ alarmValue alm
    (Just mskType) -> return $ maskTypeValue mskType
  aFcn <- (returnAlarmMaskFcn mdbc aPids ) aMDS
  mEOTC <- runDBConf mdbc $ selectFirst [OnpingTagCombinedPid ==. (Just pkey)] []
  case entityVal <$> mEOTC of
    Nothing -> return.Left $ otcErr
      where otcErr :: Text
            otcErr = "Error no OnpingTagCombined found returnMaskedOTCResult"
    (Just pOTC) -> do      
      let otcResult = onpingTagCombinedResult pOTC
          cresult = ((textToConst >=> aFcn) <$> otcResult) >>= etom
          etom (Left _) = Nothing  
          etom (Right x) = Just x      
          newOtcResult = constToText <$> cresult
      return.Right $ (pOTC {onpingTagCombinedResult = newOtcResult})

myGet :: forall (m :: * -> *) val.
               (PersistStore m, PersistEntity val,
                PersistMonadBackend m ~ PersistEntityBackend val) =>
               Maybe (Key val) -> m (Maybe val)
myGet (Just a) = do 
   get a 
myGet Nothing = return Nothing


returnAlarmMaskFcn :: MongoDBConf -> [Int] -> MaskDataStore -> IO (Const -> Either String Const)
returnAlarmMaskFcn mdbc apids amds = do
  eAtcesc <- mdsToFcn mdbc apids amds
  case eAtcesc of
    Left _ -> return (\ x -> Right $ x)
    Right acescFcn ->  return acescFcn
    
-- ============================================================

getBuiltInIdR :: Monad m => m Value
getBuiltInIdR = do
  let bil = [minBound..maxBound] :: [BuiltInId]
  return $ toJSON (L.init bil)
