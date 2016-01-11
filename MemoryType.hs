module MemoryType where


import Data.List


--MEMRORY DATA TYPE : Memories/Facts/Iteas/Things/Ect. remembered -or- worth remembering.
data MemoryMap = MemoryMap 
  { mType     :: String
  , mPath     :: String
  , mComments :: String
  , mItem     :: String  
  , mDate     :: String  
  } deriving Show

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