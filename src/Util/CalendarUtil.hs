{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util.CalendarUtil where

import           Control.Applicative
import           Data.Aeson.Types
import           Data.List
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Persist.Mongo.Settings
import           Yesod                       hiding (runDB)

-- | Helper functions to explicity do type conversion

fullDayInSeconds :: Integer
fullDayInSeconds =  86400

isOnDuty :: (MonadIO m) => MongoDBConf -> UserId -> UTCTime -> m Bool
isOnDuty mdbc uid time = do
  tz <- liftIO $ getCurrentTimeZone
  let day = utctDay time   
  let weekDay = (third $ toWeekDate day) - 1 -- the day of the week it is, formatted like the javascript counterpart
  let diffTime = utctDayTime time -- how many seconds into the day
  let timeZoneDiffTimes = secondsToDiffTime . toInteger $ (timeZoneMinutes tz) * 60
  let diffTimeWTimeZone =  fromInteger $ mod (round $ diffTime + timeZoneDiffTimes) fullDayInSeconds
  mcalObj <- liftIO $ runDBConf mdbc  $ getBy $ UniqueUserId uid
  case entityVal <$> mcalObj of
    Nothing -> do
      return False
    (Just (CalendarWidget _ _ days hours mins durations allDay)) -> do

      return $ isWithinTime timeList weekDay diffTimeWTimeZone
      where
        hourSecs = map (*3600) hours
        minSecs = map (*60) mins
        secs = zipWith (+) hourSecs minSecs
        onDutyDiffTimes = map secondsToDiffTime (map toInteger secs)
        diffTimeDurations = map secondsToDiffTime (map toInteger (map (*60) durations))
        timeList = zip4 days onDutyDiffTimes diffTimeDurations allDay

isWithinTime :: [(Int, DiffTime, DiffTime, Bool)] -> Int -> DiffTime -> Bool
isWithinTime ((day, diffStart, diffDuration, allDay):[]) targetDay targetDiffTime = (day == targetDay &&
                                                                                     diffStart <= targetDiffTime &&
                                                                                     diffStart + diffDuration >= targetDiffTime) || (allDay == True &&
                                                                                                                                     day == targetDay)
isWithinTime ((day, diffStart, diffDuration, allDay):list) targetDay targetDiffTime = (day == targetDay &&
                                                                                       diffStart <= targetDiffTime &&
                                                                                       diffStart + diffDuration >= targetDiffTime || isWithinTime list targetDay targetDiffTime ) || (allDay == True && day == targetDay)
isWithinTime [] _ _ = False

third :: (a, b, c) -> c
third (_, _, t) = t

-- | The following function is used to test if is OnDuty is working
instance ToJSON CalendarWidget where
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
