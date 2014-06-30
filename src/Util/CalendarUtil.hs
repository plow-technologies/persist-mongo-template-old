{-# LANGUAGE OverloadedStrings #-}
module Util.CalendarUtil where

import Data.Aeson.Types
import Data.Time
import Data.Time.Calendar.WeekDate
import Persist.Mongo.Settings
import Control.Applicative
import Yesod hiding (runDB)
import Data.Traversable
import Data.List

-- | Helper functions to explicity do type conversion 

isOnDuty :: MongoDBConf -> UserId -> UTCTime -> IO Bool
isOnDuty mdbc uid time = do
  tz <- getCurrentTimeZone
  let day = utctDay time
  let weekDay = (third $ toWeekDate day) - 1
  let diffTime = utctDayTime time
  let timeZoneDiffTimes = secondsToDiffTime . toInteger $ (timeZoneMinutes tz) * 60
  let diffTimeWTimeZone =  diffTime + timeZoneDiffTimes
  mcalObj <- runDBConf mdbc  $ getBy $ UniqueUserId uid
  case entityVal <$> mcalObj of
    Nothing -> do
      return False
    (Just (CalendarWidget _ _ days hours mins durations allDay)) -> return $ isWithinTime timeList weekDay diffTimeWTimeZone
      where
        hourSecs = map (*3600) hours
        minSecs = map (*60) mins
        secs = zipWith (+) hourSecs minSecs
        onDutyDiffTimes = map secondsToDiffTime (map toInteger secs)
        diffTimeDurations = map secondsToDiffTime (map toInteger (map (*60) durations))
        timeList = zip4 days onDutyDiffTimes diffTimeDurations allDay

isWithinTime :: [(Int, DiffTime, DiffTime, Bool)] -> Int -> DiffTime -> Bool
isWithinTime ((day, diffStart, diffDuration, allDay):[]) targetDay targetDiffTime = (day == targetDay && diffStart <= targetDiffTime && diffStart + diffDuration >= targetDiffTime) || (allDay == True && day == targetDay)
isWithinTime ((day, diffStart, diffDuration, allDay):list) targetDay targetDiffTime = (day == targetDay && diffStart <= targetDiffTime && diffStart + diffDuration >= targetDiffTime || isWithinTime list targetDay targetDiffTime ) || (allDay == True && day == targetDay)
isWithinTime [] _ _ = False

third :: (a, b, c) -> c
third (_, _, t) = t

-- | The following function is used to test if is OnDuty is working
instance ToJSON CalendarWidget where

testIsOnDuty :: IO ()
testIsOnDuty = do
  putStrLn "Function for testing is OnDuty\n"
  (Right mConf) <- readDBConf "config.yml"
  users <- runDBConf mConf $ selectList [] [Asc UserTagId]
  putStrLn "User List: " -- using the userTagId
  _ <- traverse (\user -> print (entityKey user, userTagName .entityVal $ user)) users
  t <- liftIO $ getCurrentTime
  let tList = t:(buildTimeList t)
  putStrLn "Day List: " 
  _ <- traverse (\tm -> print tm) tList
  putStrLn "Result List: "
  _ <- traverse (\time -> checkDailyDuty mConf users time) tList 
  putStrLn "\n =============Done=============="

checkDailyDuty :: MongoDBConf -> [Entity UserTag] -> UTCTime -> IO [()]
checkDailyDuty conf users t = do
   putStrLn "\nCurrent Date is: " >> (print t) 
   traverse (checkOnDuty conf t) users

buildTimeList :: UTCTime -> [UTCTime]
buildTimeList t = timeIncreament t <$> lst
    where lst = [1,2,3,4,5,6,7]
          timeIncreament tm n = UTCTime (addDays n (utctDay tm)) (utctDayTime tm)

checkOnDuty :: MongoDBConf -> UTCTime -> Entity UserTag -> IO ()
checkOnDuty conf t ut = do
     rslt <- isOnDuty conf (userTagUser . entityVal $ ut) t
     print (userTagName . entityVal $ ut, rslt)
--   let day = utctDay t
--   let weekDay = (third $ toWeekDate day) - 1
--   let diffTime = utctDayTime t
--   mcalObj <- runDBConf conf  $ getBy $ UniqueUserId (userTagUser . entityVal $ ut)
--   case entityVal <$> mcalObj of
--     Nothing -> do
--       print "Not found"
--     (Just (CalendarWidget _ _ days hours mins durations allDay)) -> do
--        print (days, hours, mins, durations)
--        tz <- getTimeZone t
--        let hourSecs = map (*3600) hours
--            minSecs = map (*60) mins
--            secs = zipWith (+) hourSecs minSecs
--            onDutyDiffTimes = map secondsToDiffTime (map toInteger secs)
--            diffTimeDurations = map secondsToDiffTime (map toInteger (map (*60) durations))
--            timeList = zip4 days onDutyDiffTimes diffTimeDurations allDay
--            timeZoneDiffTimes = secondsToDiffTime . toInteger $ (timeZoneMinutes tz) * 60
--            diffTimeWTimeZone = diffTime + timeZoneDiffTimes
--            rslt = isWithinTime timeList weekDay diffTimeWTimeZone
--        print (userTagName . entityVal $ ut, rslt, diffTimeWTimeZone)
--        -- print (timeList, diffTimeWTimeZone)
