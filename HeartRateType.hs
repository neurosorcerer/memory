module HeartRateType where

import Data.List 

--HEART RATE DATA TYPE
data HeartRateMap = HeartRateMap
  { hrFile     :: String
  , hrTime     :: String
  , hrComments :: String
  , hrDate     :: String
  } deriving Show



--HEART RATE MAP FUNCTIONS
--Parse HearRateMap: Parse input into HeartRateMap
parseHRMap  ::  [String] -> [HeartRateMap]
parseHRMap (hra:hrb:hrc:hrd:hrxs) = HeartRateMap {hrFile=hra, hrTime=hrb, hrComments=hrc, hrDate=hrd}:parseHRMap hrxs
parseHRMap _             = []

--Searches for work in HeartRateMap
isSearchWordInHRMap :: String -> HeartRateMap -> Bool
isSearchWordInHRMap x HeartRateMap {hrDate=shrDate}
                              =  x `isInfixOf` shrDate                          
--HearRateMap: Takes information out of HeartRateMap and puts into String.
searchHeartRateList (HeartRateMap {hrFile=slHRFile, hrTime=slHRTime, hrComments=slHRComments, hrDate=slHRDate}:xss)
             = slHRFile:slHRTime:slHRComments:slHRDate :searchHeartRateList xss
searchHeartRateList _ = []

--Heart Rate, extract information out of tuple pair for display.
hrShow (hrdata, hrtime) = ("HR- " ++ show hrdata ++ "   Time- " ++ show hrtime)
hrShow3 (hrdata, hrtime, hrdate) = ("HR- " ++ show hrdata ++ "   Time- " ++ show hrtime ++ "   Date- " ++ show hrdate)