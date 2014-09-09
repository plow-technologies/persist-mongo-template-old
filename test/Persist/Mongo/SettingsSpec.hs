module Persist.Mongo.SettingsSpec (main, spec) where

-- import Data.List ()
import           Database.Persist
import           Persist.Mongo.Settings
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "testFetchLocation" $ do
    it "should access the database for stuff" $ do
      (Right conf) <- readDBConf "config.yml"
      l <- runDBConf conf $ selectList [] [Asc DashboardId]
      (null (entityVal `fmap` l)) `shouldBe` False
