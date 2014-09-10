{-# LANGUAGE OverloadedStrings #-}
module Persist.Mongo.SettingsSpec (main, spec) where

import           Data.Aeson
import           Test.Hspec
import           TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "testParseOnpingTagCombined" $ do
    it "should be able to parse the JSON back to a OnpingTagCombined" $ do
      print $ toJSON $ parseOnpingTagCombined testOnpingTagCombinedJSON
      ((encode . parseOnpingTagCombined $ testOnpingTagCombinedJSON) == testOnpingTagCombinedJSON) `shouldBe` True
  describe "testParseOnpingTagCombinedEntity" $ do
    it "Should be able to parse an OnpingTagCombined entity " $ do
      print $ toJSON $ parseOnpingTagCombinedEntity testOnpingTagCombinedEntityJSON
      ((encode . parseOnpingTagCombinedEntity $ testOnpingTagCombinedEntityJSON) == testOnpingTagCombinedEntityJSON) `shouldBe` True

