
import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception
import Data.Time
import Control.Monad
import Data.Char
import System.Console.ANSI
import GHC.Exts
import Data.List.Split (splitOn)
import Data.Ord
import Text.Read
import MemorySection
import VoiceSection
import HeartRateSection
import WordSortSection

--MEMRORY DATA TYPE : Memories/Facts/Iteas/Things/Ect. remembered -or- worth remembering.
data MemoryMap = MemoryMap 
  { mType     :: String
  , mPath     :: String
  , mComments :: String
  , mItem     :: String  
  , mDate     :: String  
  } deriving Show

--VOICE DATA TYPE : voice recording translated into text
data VoiceMap = VoiceMap
  { vPerson    :: String
  , vContext   :: String
  , vFile      :: String
  , vComments  :: String
  , vDate      :: String
  } deriving Show 

--HEART RATE DATA TYPE
data HeartRateMap = HeartRateMap
  { hrFile     :: String
  , hrTime     :: String
  , hrComments :: String
  , hrDate     :: String
  } deriving Show

--
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

--
--HEART RATE MAP FUNCTIONS
--
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
--GENERAL FUNCTIONS

--isSearchWordInWordSort :: String -> [String] -> Bool
isSearchWordInWordSort x xs = x `isInfixOf` xs

--Zip functions For common tuple item with noncommon tuple pair
zip_zip (lista, listb) (listc, listd) = (lista, listb, listd)

--Sort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

 --Filter                            
mFilter :: (a -> Bool) ->[a] ->[a]
mFilter p xs = [ x | x <-xs, p x ]

setScreen = do
    clearScreen
    setCursorPosition 1 0

setBlueGreen = do
    setSGR [ SetColor Foreground Vivid Blue ]
    setSGR [ SetColor Background Vivid Green ]
--

--
--DISPATCH to 'These' Functions:
--
dispatch :: String -> [String] -> IO ()
dispatch "help"                 = help 
dispatch "clear"                = clear
dispatch "memory_help"          = memory_help
dispatch "memory_add"           = memory_add
dispatch "memory_view"          = memory_view
dispatch "memory_search"        = memory_search
dispatch "memory_focus"         = memory_focus
dispatch "voice_help"           = voice_help
dispatch "voice_add"            = voice_add
dispatch "voice_view"           = voice_view
dispatch "voice_search"         = voice_search
dispatch "voice_focus"          = voice_focus
dispatch "heartRate_help"       = heartRate_help
dispatch "heartRate_add"        = heartRate_add
dispatch "heartRate_search"     = heartRate_search
dispatch "heartRate_stats"      = heartRate_stats
dispatch "wordsort"             = wordsort
dispatch "searchWS"             = searchWS
dispatch "internalhelp"         = internalhelp
dispatch command  = doesntExist command

h_input args     = if null args then [] else head args
t_input args     = if null args then [] else tail args

--
--MAIN
--
main = forever $do

     putStr ("\n" ++ "Welcome to ")
     setSGR [SetColor Foreground Vivid Magenta]
     putStr "MEMORY " 
     setSGR [Reset] 
     putStr ("Dr. Worrell." ++ "\n" ++ "Please enter command -or- ")
     setSGR [ SetConsoleIntensity BoldIntensity ] 
     putStr "help"
     setSGR [ SetConsoleIntensity NormalIntensity ]
     putStr  (" for help menu: " ++ "\n")
     setSGR [Reset] 

     input <- getLine
     let inputList = words input
         command   = h_input inputList  
         argList   = t_input inputList
     dispatch command argList

clear _ = do 
     clearScreen
     setCursorPosition 1 0  


help _ = do
     clearScreen
     setCursorPosition 1 0
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStrLn ("\n" ++ "HELP MENU:"++"\n")
     setSGR [ SetUnderlining SingleUnderline ]      
     putStrLn "Main Help:"
     setSGR [Reset]                    
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 " memory_help  " 
     setSGR [Reset]
     putStr ("    for further help with") 
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr " personal memory"
     setSGR [Reset]
     putStr (":     |" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 " voice_help  " 
     setSGR [Reset]
     putStr ("     for further help with")
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr " voice memory"
     setSGR [Reset]
     putStr (":        |" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 " heartRate_help  " 
     setSGR [Reset]
     putStr (" for further help with")
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr " heart rate data"
     setSGR [Reset]
     putStr (":     |" ++ "\n" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 " clear" 
     setSGR [Reset]
     putStr                 ("            to clear screen                            |" ++ "\n" ++ "\n")

doesntExist command _= do
     clearScreen
     setCursorPosition 1 0
     putStrLn $ "Sorry Dr. Worrell, the "++command++" command doesn't exist."
































