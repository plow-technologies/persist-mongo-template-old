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
import Mask.BuiltIns
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

testCompile :: String
testCompile = "tst"
 
-- | Add your builtIn Functions here, BuiltIns and in Types

maskLookup :: MongoDBConf -> MaskData  -> IO MaskFcn
maskLookup _ (MaskData DivByTen  _) =  return $ OneVar $ divBy10
maskLookup _ (MaskData DivBy100  _) =  return $ OneVar $ divBy100
maskLookup _ (MaskData MultByTen  _) =  return $ OneVar $ multBy10
maskLookup _ (MaskData MultBy100  _) =  return $ OneVar $ multBy100

-- | maskLookup for Bit Operation Functions
maskLookup _ (MaskData Bit0 _) =  return $ OneVar $ bit0
maskLookup _ (MaskData Bit1 _) =  return $ OneVar $ bit1
maskLookup _ (MaskData Bit2 _) =  return $ OneVar $ bit2
maskLookup _ (MaskData Bit3 _) =  return $ OneVar $ bit3
maskLookup _ (MaskData Bit4 _) =  return $ OneVar $ bit4
maskLookup _ (MaskData Bit5 _) =  return $ OneVar $ bit5
maskLookup _ (MaskData Bit6 _) =  return $ OneVar $ bit6
maskLookup _ (MaskData Bit7 _) =  return $ OneVar $ bit7
maskLookup _ (MaskData Bit8 _) =  return $ OneVar $ bit8
maskLookup _ (MaskData Bit9 _) =  return $ OneVar $ bit9
maskLookup _ (MaskData Bit10 _) =  return $ OneVar $ bit10
maskLookup _ (MaskData Bit11 _) =  return $ OneVar $ bit11
maskLookup _ (MaskData Bit12 _) =  return $ OneVar $ bit12
maskLookup _ (MaskData Bit13 _) =  return $ OneVar $ bit13
maskLookup _ (MaskData Bit14 _) =  return $ OneVar $ bit14
maskLookup _ (MaskData Bit15 _) =  return $ OneVar $ bit15


-- | maskLookup for invBit Operation Functions
maskLookup (MaskData InvBit0 _) =  return $ OneVar $ invBit0
maskLookup (MaskData InvBit1 _) =  return $ OneVar $ invBit1
maskLookup (MaskData InvBit2 _) =  return $ OneVar $ invBit2
maskLookup (MaskData InvBit3 _) =  return $ OneVar $ invBit3
maskLookup (MaskData InvBit4 _) =  return $ OneVar $ invBit4
maskLookup (MaskData InvBit5 _) =  return $ OneVar $ invBit5
maskLookup (MaskData InvBit6 _) =  return $ OneVar $ invBit6
maskLookup (MaskData InvBit7 _) =  return $ OneVar $ invBit7
maskLookup (MaskData InvBit8 _) =  return $ OneVar $ invBit8
maskLookup (MaskData InvBit9 _) =  return $ OneVar $ invBit9
maskLookup (MaskData InvBit10 _) =  return $ OneVar $ invBit10
maskLookup (MaskData InvBit11 _) =  return $ OneVar $ invBit11
maskLookup (MaskData InvBit12 _) =  return $ OneVar $ invBit12
maskLookup (MaskData InvBit13 _) =  return $ OneVar $ invBit13
maskLookup (MaskData InvBit14 _) =  return $ OneVar $ invBit14
maskLookup (MaskData InvBit15 _) =  return $ OneVar $ invBit15


-- | maskLookup for Identity Function
maskLookup _ (MaskData Identity _) =  return $ OneVar $ identity

-- | maskLookup for UserDefined Function
maskLookup mdbc (MaskData UserDefined  um) =  UserDef <$> (tagMapTransform mdbc um)
-- maskLookup (MaskData _  um ) =  return $ OneVar $ (\x -> Right x)
    
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
keyTraverseFcn mdbc _  (TagTarget i _ ) = do
  let eTextToDouble :: Entity OnpingTagCombined  -> Maybe Double
      eTextToDouble = (onpingTagCombinedResult.entityVal) >=> (readMaybe.unpack)
  v <- runDBConf mdbc $ selectFirst [OnpingTagCombinedPid ==. (Just i)] []
  case (v >>= eTextToDouble) of
    Just opv -> return (ConstDouble opv)
    Nothing  -> fail "No value found"


tagMapTransform :: MongoDBConf -> UserMask -> IO (Const -> Either String Const)
tagMapTransform mdbc (UserMask stmt tm)  = do
  vtReady <- M.traverseWithKey (keyTraverseFcn mdbc) tm
  return $ runner vtReady
    where
      runner vt x = let vtNew = VT (M.insert "input1" x vt)
                    in (sstEval vtNew stmt ) >>= sstLookupOutput

insertPidKeys :: UserMask -> [Int] -> UserMask
insertPidKeys (UserMask stmt _) pList = do 
  let tFcn x = TagTarget x TagCombined
      iFcn x = append "input" (pack.show $ x)
  UserMask stmt (M.fromList [(iFcn i, tFcn p) | (i,p) <- (zip ( [1 ..]::[Int]) pList) ] )


mkUserMask :: Stmt -> UserMask 
mkUserMask stmts = emptyUserMask { getStmtTree = stmts}

mkMaskDataStore :: BuiltInId -> UserMask -> MaskDataStore 
mkMaskDataStore b u = maskDataEncode $ MaskData b u

maskDataDecode :: MaskDataStore -> Either String MaskData
maskDataDecode (MaskDataStore maskBS) = runGet safeGet maskBS

maskDataEncode :: MaskData -> MaskDataStore 
maskDataEncode md = MaskDataStore $ runPut $ safePut $ md

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
                         inputValues :: [Integer], -- Test values used in mask creation
                         inputNames  :: [Text],    -- Names of various input labels in order
                         workingCode ::  Text --structured script code body
} deriving (Show, Eq, Generic)

instance A.ToJSON SSTConfig
instance A.FromJSON SSTConfig


testStructuredScript :: SSTConfig
testStructuredScript = SSTConfig [] [] ""




decodeMaskAssignConfig :: A.Value -> A.Result MaskAssignConfig
decodeMaskAssignConfig = A.fromJSON
     
data MaskAssignConfig = MaskAssignConfig { 
  macMaskTypeId :: Maybe MaskTypeId,
  macKeys       :: [Int],
  macBuiltIn    :: BuiltInId,
  macDefault    :: Bool,
  macName       :: Maybe Text
  }
   deriving (Show,Eq,Generic)


instance ToJSON MaskAssignConfig where 
instance FromJSON MaskAssignConfig where 

maskAssignConfigToMaskTypeJoin :: MongoDBConf -> MaskAssignConfig -> IO (Either Text MaskTypeJoin) 
maskAssignConfigToMaskTypeJoin _ (MaskAssignConfig _ [] _ _ _) = do
  let err :: Text  
      err = "no keys present"
  return.Left $ err  
maskAssignConfigToMaskTypeJoin mdbc (MaskAssignConfig mmtid keys@(primaryKey:_) bid dflt mname) = do
  _ <- resetMTJDefault mdbc primaryKey dflt
  case mmtid of
    Nothing -> do
      ey <- (makeBuiltInDataStore keys bid)
      return $ ey >>= (\y -> return $ MaskTypeJoin y mmtid primaryKey dflt mname)
    (Just mtid) -> do 
      ex <- (makeUserDefDataStore mdbc keys mtid) 
      return $ ex >>= (\x -> return $ MaskTypeJoin x mmtid primaryKey dflt mname)

makeBuiltInDataStore :: [Int] -> BuiltInId -> IO (Either Text MaskDataStore)
makeBuiltInDataStore keys binId = do
  let bium = insertPidKeys emptyUserMask keys
  return (return $ mkMaskDataStore binId bium)
  
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
getMaskFunctionDefault mdbc pid = do
  mtje <- runDBConf mdbc $ selectFirst [(MaskTypeJoinPKey ==. pid),(MaskTypeJoinDefaultSelect ==. True)] []
  case mtje of
    Nothing -> do
      let err :: Text
          err = "No defaultMaskTypeJoin found"
      return $ Left err
    (Just mtj) -> do 
      let mds = maskTypeJoinValue.entityVal $ mtj
          edecodeMDS = maskDataDecode mds
          decodedMDS = (eStringToEText edecodeMDS) :: Either Text MaskData     
      edecodeUM <- T.traverse (maskLookup mdbc) decodedMDS 
      return $ maskPullOut <$>  edecodeUM

-- insertPidKeys :: UserMask -> [Int] -> UserMask

mdsToFcn :: MongoDBConf ->[Int] -> MaskDataStore -> IO (Either Text (Const -> Either String Const ))
mdsToFcn mdbc pids mds = do 
  let emd =  (eStringToEText.maskDataDecode $ mds)
      eum = ( (\um -> insertPidKeys um pids ) . userMask) <$> emd
      emd' = eum >>= (\um ->  emd >>= (\md -> return $ md{userMask= um}))
  (T.traverse (maskLookup mdbc) emd') >>= (\x -> return $ maskPullOut <$> x)

  

onpingTagCombinedDefaultTransform :: MongoDBConf -> OnpingTagCombined -> IO OnpingTagCombined
onpingTagCombinedDefaultTransform mdbc otc = do      
  let (Just pid) = onpingTagCombinedPid otc
      result = onpingTagCombinedResult otc
  fcn <- returnDefaultMaskFunction mdbc pid
  let constresult = ((textToConst >=> fcn) <$> result) >>= etom
      etom (Left _) = Nothing  
      etom (Right x) = Just x
  let newresult = constToText <$> constresult
  return $ (otc {onpingTagCombinedResult = newresult})
    
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
extracted mdbc uniquePids = do
   uniqueCFcns <- T.traverse (returnDefaultMaskFunction mdbc) uniquePids
   
   return $ onpingTagHWrapper <$> uniqueCFcns


             

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
let bil=[minBound..maxBound] :: [BuiltInId]
return $ toJSON (L.init bil)
