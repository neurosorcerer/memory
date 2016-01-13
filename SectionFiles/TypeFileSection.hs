--MEMRORY DATA TYPE : Memories/Facts/Iteas/Things/Ect. remembered -or- worth remembering.

module TypeFileSection where 


data MemoryMap = MemoryMap 
  { mType     :: String
  , mPath     :: String
  , mComments :: String
  , mItem     :: String  
  , mDate     :: String  
  } deriving Show

--HEART RATE DATA TYPE
data HeartRateMap = HeartRateMap
  { hrFile     :: String
  , hrTime     :: String
  , hrComments :: String
  , hrDate     :: String
  } deriving Show

--VOICE DATA TYPE : voice recording translated into text
data VoiceMap = VoiceMap
  { vPerson    :: String
  , vContext   :: String
  , vFile      :: String
  , vComments  :: String
  , vDate      :: String
  } deriving Show 



