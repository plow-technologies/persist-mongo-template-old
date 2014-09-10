{-# LANGUAGE OverloadedStrings #-}
module Persist.Mongo.SettingsSpec (main, spec) where

import           Data.Aeson
import           Database.Persist
import           Persist.Mongo.Settings
import           Test.Hspec
import           TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "testParseOnpingTagCombined" $ do
    it "should be able to parse the JSON back to a OnpingTagCombined" $ do
      ((encode . parseOnpingTagCombined $ testOnpingTagCombinedJSON) == testOnpingTagCombinedJSON) `shouldBe` True
  describe "testParseOnpingTagCombinedEntity" $ do
    it "Should be able to parse an OnpingTagCombined entity " $ do
      ((encode . parseOnpingTagCombinedEntity $ testOnpingTagCombinedEntityJSON) == testOnpingTagCombinedEntityJSON) `shouldBe` True

