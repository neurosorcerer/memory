module ProgramFunctions where

--MEMORY MAP FUNCTIONS
--
--Parse Memory Map: Parse input into MemoryMap 
parseMemoryMap :: [String] -> [MemoryMap]  
parseMemoryMap (a:b:c:d:e:xs) = MemoryMap {mType=a, mPath=b, mComments=c, mItem=d, mDate=e}:parseMemoryMap xs
parseMemoryMap _              = []

--Searches for work in MemoryMap
isSearchWordInMap :: String -> MemoryMap -> Bool
isSearchWordInMap x MemoryMap {mType=sType, mPath=sPath, mComments=sComments, mItem=sItem, mDate=sDate}
                              = x `isInfixOf` sType  || x `isInfixOf` sPath 
                             || x `isInfixOf` sComments || x `isInfixOf` sItem 
                             || x `isInfixOf` sDate

--MemoryMap: Takes information out of MemoryMap and puts into String.
searchList (MemoryMap {mType=slType, mPath=slPath, mComments=slComments, mItem=slItem, mDate=slDate}:xss)
             = slType:slPath:slComments:slItem:slDate :searchList xss
searchList _ = []

--VOICE MAP FUNCTIONS
--
--Parse VoiceMap: Parse input into VoiceMap
parseVoiceMap  ::  [String] -> [VoiceMap]
parseVoiceMap (va:vb:vc:vd:ve:vxs) = VoiceMap {vPerson=va, vContext=vb, vFile=vc, vComments=vd, vDate=ve}:parseVoiceMap vxs
parseVoiceMap _             = []

--Searches for work in VoiceMap
isSearchWordInVoiceMap :: String -> VoiceMap -> Bool
isSearchWordInVoiceMap x VoiceMap {vPerson=svPerson, vContext=svContext, vFile=svFile, vComments=svComments, vDate=svDate}
                              = x `isInfixOf` svPerson  || x `isInfixOf` svContext 
                             || x `isInfixOf` svFile || x `isInfixOf` svComments 
                             || x `isInfixOf` svDate       

--VoiceMap: Takes information out of VoiceMap and puts into String.
searchVoiceList (VoiceMap {vPerson=slPerson, vContext=slContext, vFile=slFile, vComments=slComments, vDate=slDate}:xss)
             = slPerson:slContext:slFile:slComments:slDate :searchVoiceList xss
searchVoiceList _ = []



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
--