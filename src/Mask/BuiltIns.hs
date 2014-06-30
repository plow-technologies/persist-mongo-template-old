{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Mask.BuiltIns where 

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)

import Language.StructuredScript.Parsers
import Data.Bits
--import Text.Julius
--import qualified Data.Aeson as A



testCompile :: String
testCompile = "tst"

-- | Add your builtIn Functions here and in Mask.hs and in Types.hs




  
divBy10 :: Const -> Either String Const
divBy10 (ConstDouble x) = Right $ ConstDouble $ x / 10.0
divBy10 _ = Left $ "Expected Double"

multBy10 :: Const -> Either String Const
multBy10 (ConstDouble x) = Right $ ConstDouble $ x * 10.0
multBy10 _ = Left $ "Expected Double"

divBy100 :: Const -> Either String Const
divBy100 (ConstDouble x) = Right $ ConstDouble $ x / 100.0
divBy100 _ = Left $ "Expected Double"


multBy100 :: Const -> Either String Const
multBy100 (ConstDouble x) = Right $ ConstDouble $ x * 100.0
multBy100 _ = Left $ "Expected Double"

-- | BuiltIn functions for Bit Operations

bit0 :: Const -> Either String Const
bit0 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 0
bit0 _ = Left $ "Expected Double"

bit1 :: Const -> Either String Const
bit1 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 1
bit1 _ = Left $ "Expected Double"

bit2 :: Const -> Either String Const
bit2 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 2
bit2 _ = Left $ "Expected Double"

bit3 :: Const -> Either String Const
bit3 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 3
bit3 _ = Left $ "Expected Double"

bit4 :: Const -> Either String Const
bit4 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 4
bit4 _ = Left $ "Expected Double"

bit5 :: Const -> Either String Const
bit5 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 5
bit5 _ = Left $ "Expected Double"

bit6 :: Const -> Either String Const
bit6 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 6
bit6 _ = Left $ "Expected Double"

bit7 :: Const -> Either String Const
bit7 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 7
bit7 _ = Left $ "Expected Double"

bit8 :: Const -> Either String Const
bit8 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 8
bit8 _ = Left $ "Expected Double"

bit9 :: Const -> Either String Const
bit9 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 9
bit9 _ = Left $ "Expected Double"

bit10 :: Const -> Either String Const
bit10 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 10
bit10 _ = Left $ "Expected Double"

bit11 :: Const -> Either String Const
bit11 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 11
bit11 _ = Left $ "Expected Double"

bit12 :: Const -> Either String Const
bit12 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 12
bit12 _ = Left $ "Expected Double"

bit13 :: Const -> Either String Const
bit13 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 13
bit13 _ = Left $ "Expected Double"

bit14 :: Const -> Either String Const
bit14 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 14
bit14 _ = Left $ "Expected Double"

bit15 :: Const -> Either String Const
bit15 (ConstDouble x) = Right $ ConstBool $ testBit ((floor x)::Int) 15
bit15 _ = Left $ "Expected Double"


invBit0 :: Const -> Either String Const
invBit0 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 0
invBit0 _ = Left $ "Expected Double"

invBit1 :: Const -> Either String Const
invBit1 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 1
invBit1 _ = Left $ "Expected Double"

invBit2 :: Const -> Either String Const
invBit2 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 2
invBit2 _ = Left $ "Expected Double"

invBit3 :: Const -> Either String Const
invBit3 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 3
invBit3 _ = Left $ "Expected Double"

invBit4 :: Const -> Either String Const
invBit4 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 4
invBit4 _ = Left $ "Expected Double"

invBit5 :: Const -> Either String Const
invBit5 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 5
invBit5 _ = Left $ "Expected Double"

invBit6 :: Const -> Either String Const
invBit6 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 6
invBit6 _ = Left $ "Expected Double"

invBit7 :: Const -> Either String Const
invBit7 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 7
invBit7 _ = Left $ "Expected Double"

invBit8 :: Const -> Either String Const
invBit8 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 8
invBit8 _ = Left $ "Expected Double"

invBit9 :: Const -> Either String Const
invBit9 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 9
invBit9 _ = Left $ "Expected Double"

invBit10 :: Const -> Either String Const
invBit10 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 10
invBit10 _ = Left $ "Expected Double"

invBit11 :: Const -> Either String Const
invBit11 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 11
invBit11 _ = Left $ "Expected Double"

invBit12 :: Const -> Either String Const
invBit12 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 12
invBit12 _ = Left $ "Expected Double"

invBit13 :: Const -> Either String Const
invBit13 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 13
invBit13 _ = Left $ "Expected Double"

invBit14 :: Const -> Either String Const
invBit14 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 14
invBit14 _ = Left $ "Expected Double"

invBit15 :: Const -> Either String Const
invBit15 (ConstDouble x) = Right $ ConstBool $ not $ testBit ((floor x) :: Int) 15
invBit15 _ = Left $ "Expected Double"

-- | BuiltIn functions for returning Identity
identity :: Const -> Either String Const
identity (ConstDouble x) = Right $ ConstDouble $ x
identity _ = Left $ "Expected Double"

