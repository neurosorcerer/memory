module VoiceType where


import Data.List


--VOICE DATA TYPE : voice recording translated into text
data VoiceMap = VoiceMap
  { vPerson    :: String
  , vContext   :: String
  , vFile      :: String
  , vComments  :: String
  , vDate      :: String
  } deriving Show 


--
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
