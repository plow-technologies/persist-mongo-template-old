{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Mask.Types (module Plowtech.Persist.Mask.Types) where

import           Prelude                           hiding (head, init, last,
                                                    readFile, tail, writeFile)


import qualified Data.Hashable                     as H
import qualified Data.HashMap.Strict               as M
import           Data.String                       (IsString)
import           Data.Text
import           Plowtech.Persist.Mask.Types

-- | /mask/editor MaskEditorR T
-- | CR -> "MaskEditorCR"


-- | Pick either a Tag result from Onping Tag History
-- | Or OnpingTagCombined


-- | TargetId is left as a generic Type variable in case I decide the best
--   way to store it is by storing the query itself (the middle part of a selectFirst or selectList)

-- | This is the input map which is keyed by whatever is indexing
-- It is transformed into the destination target by TagTransform
-- Tag transform will represent a class of functions that can
-- take a TagImap to some f (varTable)

type TagIMap a = (Show a, Eq a) => M.HashMap Text (TagTarget  a)


sampleTagIMap :: forall k a.
                       (Eq k, Num a, Data.String.IsString k, H.Hashable k) =>
                       M.HashMap k (TagTarget a)
sampleTagIMap = M.insert "input1" (TagTarget 299 TagHistory) M.empty


-- |User Mask is envisioned to work like this on run ...
{-| get The 'UserMaskID', and Retrieve this type
    if the 'VarTable' is empty build a new VTable with the
    'TagTarget' , using a 'TagTransform' Function
    Then, a function is constructed that takes input
    and runs it against the VarTable and the Stmt Tree
   This is the output of the Tag Transform and can then be applicatively
   mapped with your input parameter.
   Some Notes,
   * All Parameters BUT THE FIRST are looked up using OnpingTagCombined
     +This may change for now but it is currently beyond the scope
      of SST to handle mapping and pairing times with inputs


|-}


-- (TagTargetTransform a b)
-- (TagTarget a -> TagTargetTransform a b)
-- type TagTargetTransform a b = forall m .(Monad m, Functor m) => TagTarget a ->  ( b -> m b)



-- | Add your builtIn Functions here and in Mask.hs and in Mask.hs
-- | Always leave UserDefined as the last in BuiltInId type so the getBuilInIdR can strip it off.


-- | Add Built In Masks that you have created to this list so they may be created in



-- | Stored in the DB as MaskDataStore
