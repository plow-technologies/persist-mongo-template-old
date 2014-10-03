{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SafeCopy where 
import Data.Serialize
import Data.SafeCopy
import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)
import Data.ByteString
--import Text.Julius
--import qualified Data.Aeson as A

import Plowtech.Persist.Mask()

{-| When there are Types that don't really fit 
into Yesod's Persist DB engine, a Safecopy is serialized here.

Then the appropriate thing to do is wrap the serialized Bytestring in a type generated 
in the model file.  

That will allow the information to still maintain some type safety in addition to 
the new version information.
|-}



data TestSafeCopy = TestSafeCopy { unTest :: Int } 
      deriving (Read,Eq,Show)

$(deriveSafeCopy 0 'base ''TestSafeCopy)

testSafeCopyPut :: ByteString
testSafeCopyPut = runPut $ safePut $ TestSafeCopy 3

testSafeCopyGet :: Either String TestSafeCopy  
testSafeCopyGet = runGet safeGet testSafeCopyPut 
