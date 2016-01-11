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
--
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

--
--START VOICEMAP SECTION::
--

voice_help _ = do
     clearScreen
     setCursorPosition 1 0
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStrLn ("\n" ++ "VOICE HELP MENU:"++"\n"++"\n")
     setSGR [ SetUnderlining SingleUnderline ]      
     putStrLn "VOICE COMMANDS:"
     setSGR [Reset]                    
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 "   voice_add" 
     setSGR [Reset]
     putStr                 ("      to add new voice memory file                   |" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 "   voice_view" 
     setSGR [Reset]
     putStr                 ("     to view all voice memory files                 |" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 "   voice_search" 
     setSGR [Reset]
     putStr                 ("   to search past voice memories file             |" ++ "\n" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 "   voice_focus" 
     setSGR [Reset]
     putStr                 ("    to refine search -or focused- voice memories   |" ++ "\n" ++
                             "       --restart search when focus returns no memories--" ++ "\n" ++
                            "\n"
                           )
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr "Enter a Command from "
     setSGR [ SetUnderlining SingleUnderline ] 
     putStr "VOICE COMMANDS"
     setSGR [ SetUnderlining NoUnderline ]
     putStr  (" and follow prompt:" ++ "\n")
     setSGR [Reset]

voice_add _  = do 
     clearScreen
     setCursorPosition 1 0
     putStr "Who's Voice is this?: " 
     vPerson        <- getLine 
     putStr "What is the Context of this recording?: "
     vContext       <- getLine  
     putStr "Please enter FileName where translated -voice to text- recording is saved : "
     vFile          <- getLine 
     putStr "Any Comments -enter later to enter commetns later: "
     vComments      <- getLine
     cTime          <- getCurrentTime 

     fileExists     <- doesFileExist vFile
     if fileExists
        then do contents <- readFile vFile
                let voiceMapList    = [vPerson, vContext, vFile, vComments]
                    mapVoiceString  = unlines voiceMapList
                    (y,m,d)         = toGregorian $ utctDay cTime
                setSGR [ SetColor Foreground Vivid Blue ]
                putStrLn ("\n" ++ mapVoiceString ++ show (y,m,d) ++ "\n")
                setSGR [Reset]
                putStr ("Do you wish to save this Vocal Memory (yes/no)?: ")
                saveYesNo      <- getLine
                if saveYesNo        == "yes"
                   then do putStrLn ("\n" ++ "Voice Memory established!" ++ "\n")
                           appendFile "workingVoiceMemory.txt" (mapVoiceString ++ show (y,m,d) ++"\n")     
                else do putStr "Ok. Thank You!"
        else do putStr ("\n"++ "That Voice File does not exist:" ++ "\n")

voice_view _ = do
    clearScreen
    setCursorPosition 1 0
    contents     <- readFile "workingVoiceMemory.txt"
    putStrLn ("\n" ++ "Your Voice Memories Dr. Worrell:" ++ "\n")
    setSGR [ SetColor Foreground Vivid Blue ]
    setSGR [ SetColor Background Vivid Cyan ]
    putStrLn contents
    setSGR [ Reset ]
    putStr ("\n" ++ "Press any key to clear screen and return to Main Menu:")
    key <- getLine
    clearScreen
    setCursorPosition 1 0

voice_search _ = do

     clearScreen
     setCursorPosition 1 0
     fileExists  <- doesFileExist "workingVoiceMemory.txt"
     if fileExists
        then do memoryLines <- readFile "workingVoiceMemory.txt"
                putStr      "Enter Voice Memory Search Term: " 
                searchword  <- getLine

                let lowerMemory        = map toLower memoryLines
                    listMemoryLines    = lines lowerMemory
                    lowerSearchWord    = map toLower searchword

                    mMapList           = parseVoiceMap listMemoryLines
                    searchMapPosition  = mFilter (isSearchWordInVoiceMap lowerSearchWord) mMapList
         
                    searchListPosition = searchVoiceList searchMapPosition
                    organizeSMP        = ("\n" ++ "Your Voice Memories Dr. Worrell:" ++ 
                                          "\n" ++ unlines searchListPosition
                                         )
                    tempMemorySMP      = unlines searchListPosition
                if searchListPosition == []
                   then putStrLn ("\n"++ "Sorry, search term returned zero results.")      
                   else do writeFile "temporaryVoiceMemory.txt" (tempMemorySMP)
                           setSGR [ SetColor Foreground Vivid Blue ]
                           setSGR [ SetColor Background Vivid Cyan ]
                           putStrLn organizeSMP
                           setSGR [ Reset ]
     else putStr ("\n"++ "ERROR: Voice Memory folder is missing. Please return folder to Memory program:" ++ "\n") 

voice_focus _ =  do
     tempMemoryFile <- readFile "temporaryVoiceMemory.txt"
     putStr $"Enter a refined search term to focus your voice memory Dr. Worrell: "
     refine <- getLine
      
     let lowerTempMemory        = map toLower tempMemoryFile
         listTempMemory         = lines lowerTempMemory
         lowerRefineWord        = map toLower refine

         mMapTempList           = parseVoiceMap listTempMemory
         searchMapTempPosition  = mFilter (isSearchWordInVoiceMap lowerRefineWord) mMapTempList
         
         searchListTempPosition = searchVoiceList searchMapTempPosition
         organizeTempSMP        = ("\n" ++ "Your Voice Memories Dr. Worrell:" ++ 
                                   "\n" ++ unlines searchListTempPosition
                                  )

         tempFocusMemorySMP      = unlines searchListTempPosition
     if searchListTempPosition == []
        then putStrLn ("\n"++ "Sorry, focused search term returned zero results.") 
        else do setSGR [ SetColor Foreground Vivid Blue ]
                setSGR [ SetColor Background Vivid Cyan ]
                putStrLn organizeTempSMP
                setSGR [ Reset ]
                writeFile "temporaryVoiceMemory.txt" (tempFocusMemorySMP)
                putStr ("\n" ++ "Press any key to clear screen and return to Main Menu:")
                key <- getLine
                clearScreen
                setCursorPosition 1 0


--
--START HEARTRATEMAP SECTION::
--

heartRate_help _ = do
     clearScreen
     setCursorPosition 1 0
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStrLn ("\n" ++ " HEART RATE HELP MENU:"++"\n"++"\n")
     setSGR [ SetUnderlining SingleUnderline ]      
     putStrLn "HEART RATE COMMANDS:"
     setSGR [Reset]  
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]                  
     putStr                 "  heartRate_add" 
     setSGR [Reset]
     putStr                 ("     to add new Heart Rate Data File                      |" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 "  heartRate_search" 
     setSGR [Reset]
     putStrLn                 ("  to search past Heart Rate Data Files                 |" ++ "\n")
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr "Enter a Command from "
     setSGR [ SetUnderlining SingleUnderline ] 
     putStr "HEART RATE COMMANDS"
     setSGR [ SetUnderlining NoUnderline ]
     putStr  (" and follow prompt:" ++ "\n")
     setSGR [Reset]

--Add Heart Rate File
heartRate_add _  = do 
     clearScreen
     setCursorPosition 1 0
     putStr "Enter Heart Rate Date File: " 
     hrAddFile         <- getLine  
     fileExists     <- doesFileExist hrAddFile
     if fileExists
        then do contData <- readFile hrAddFile
                putStr "Enter Coresponding Time File: " 
                hrAddTime         <- getLine  
                fileExists     <- doesFileExist hrAddTime
                if fileExists
                   then do contTime <- readFile hrAddTime
                           putStrLn ("Data Files Uploaded." ++ "\n")
                           putStr "Enter Comments Regarding Heart Rate File -enter 'later' to enter comments later: "
                           hrComments    <- getLine    
                           putStr "Enter Date of Heart Rate Recording (ex. 27Dec15): "    
                           hrDate       <- getLine
                           let hrMapList = (contData ++ "-splitfile-" ++ contTime ++ "-splitfile-" ++ 
                                            hrComments ++ "-splitfile-" ++ hrDate ++ "-splitfile-")  
                           setSGR [ SetColor Foreground Vivid Blue ]
                           appendFile "workingHeartRate.txt" (hrMapList)
                           setSGR [Reset]
                           putStr ("\n" ++ "Do you wish to save this Heart Rate Data File (yes/no)?: ")
                           saveYesNo      <- getLine
                           if saveYesNo        == "yes"
                           then do putStrLn ("\n" ++ "Heart Rate Data File established!" ++ "\n")
         

                           else do putStr ("\n" ++ "Ok. Heart rate information was NOT saved. Thank You!" ++ "\n")
                else do putStrLn ("\n"++ "That Heart Rate File does not exist:" ++ "\n")              
        else do putStr ("\n"++ "That Heart Rate File does not exist:" ++ "\n")
                

heartRate_search _ = do
     clearScreen
     setCursorPosition 1 0
     fileExists    <- doesFileExist "workingHeartRate.txt"
     if fileExists
        then do heartRateFile <- readFile "workingHeartRate.txt"
                putStr      "Enter Date of heart rate file you would like to view -ex. (ex. 27Dec15): " 
                searchDate    <- getLine 
                let hrfSplitOn                = splitOn "-splitfile-" heartRateFile     
                    hrMapList                 = parseHRMap hrfSplitOn                               
                    searchMapPosition         = mFilter (isSearchWordInHRMap searchDate) hrMapList  
                    hrFileList                = map hrFile     searchMapPosition
                    hrTimeList                = map hrTime     searchMapPosition
                    hrCommentsList            = map hrComments searchMapPosition
                    hrDateList                = map hrDate     searchMapPosition
                    hrFLul                    = unlines hrFileList
                    hrTLul                    = unlines hrTimeList
                    shrFileWords              = words hrFLul
                    shrTimeWords              = words hrTLul 
                    shrTimeParseResults       = map readEither shrTimeWords :: [Either String Float]
                    shrTimeFloats             = map (either (const (-999)) id) shrTimeParseResults
                    shrTimeFloatsHour         = map (/3598.379) shrTimeFloats
                    shrFileParseResults       = map readEither shrFileWords :: [Either String Float]
                    shrFileFloats             = map (either (const (-999)) id) shrFileParseResults
                    hrDTFloatZip              = zip shrFileFloats shrTimeFloatsHour
                    hrDT                      = zip shrFileWords shrTimeWords
                    hrDTSMap                  = map hrShow hrDT
                    hrDTSMapZip               = map hrShow hrDTFloatZip
                    hrDTSul                   = unlines hrDTSMap -- <- full list of tuple (HR, Time)...
                    hrDTSulZip                = unlines hrDTSMapZip

                    maxHRFloatZip             = maximumBy (comparing fst) hrDTFloatZip
                    minHRFloatZip             = minimumBy (comparing fst) hrDTFloatZip
                    maxHRShowFloatZip         = hrShow maxHRFloatZip
                    minHRShowFloatZip         = hrShow minHRFloatZip
                    hrFloatZipStats           = ("\n" ++ "Heart Rate Stats for " ++ searchDate ++
                                                 "\n" ++ "\n" ++
                                                 "Max " ++ maxHRShowFloatZip ++ "\n" ++
                                                 "Min " ++ minHRShowFloatZip ++ "\n" ++ "\n" ++
                                                 "Comments: " ++ unlines hrCommentsList ++ "\n")                  
                
                    organizeSMP               = ("\n" ++ "Your Heart Rate Dr. Worrell:" ++ 
                                                 "\n" ++ hrDTSul ++ "\n"
                                                 ++ "Comments: " ++ unlines hrCommentsList ++ "\n"
                                                 ++ unlines hrDateList
                                                )
                if hrDateList == []
                   then do putStrLn ("\n"++ "Sorry, there is no Heart Rate data from that date.")      
                   else do writeFile "temporaryWorkingHeartRate.txt" (hrDTSulZip)
                           setSGR [ SetColor Foreground Vivid Blue ]
                           setSGR [ SetColor Background Vivid Magenta ]
                           --putStrLn organizeSMP
                           putStrLn hrFloatZipStats                          
                           setSGR [ Reset ]
        else do putStr ("\n"++ "ERROR: Heart Rate folder is missing. Please return folder to Memory program:" ++ "\n")

--
--
--Starting to add mathmatical and useful functiuons to heartRate. 
--HeartRate_stats, give max and min for all days listed.
--heartRate_avg, avg all time points form all HR recordings.

-- Editing here NOW. Added heartRate_stats to Dispatch and now adding function. 
-- Stats will give max and min and value and time of day for every HR file entered
heartRate_stats _ = do
     clearScreen
     setCursorPosition 1 0
     fileExists    <- doesFileExist "workingHeartRate.txt"
     if fileExists
        then do heartRateFile <- readFile "workingHeartRate.txt"   
                let hrfSplitOn                = splitOn "-splitfile-" heartRateFile     
                    hrMapList                 = parseHRMap hrfSplitOn     
                    hrFileList                = map hrFile     hrMapList
                    hrTimeList                = map hrTime     hrMapList
                    hrCommentsList            = map hrComments hrMapList
                    hrDateList                = map hrDate     hrMapList  
                    hrFLul                    = unlines hrFileList
                    hrTLul                    = unlines hrTimeList
                    hrDLul                    = unlines hrDateList
                    shrFileWords              = words hrFLul
                    shrTimeWords              = words hrTLul 
                    shrDateWords              = words hrDLul 
                    shrTimeParseResults       = map readEither shrTimeWords :: [Either String Float]
                    shrTimeFloats             = map (either (const (-999)) id) shrTimeParseResults
                    shrTimeFloatsHour         = map (/3598.379) shrTimeFloats
                    shrFileParseResults       = map readEither shrFileWords :: [Either String Float]
                    shrFileFloats             = map (either (const (-999)) id) shrFileParseResults
                    --
                    -- reBuilding actual Global stats here ...extention of {basic copy} below
                    

                    
                               --Gives Global Max and Min value--
                    hrDTFloatZip              = zip shrFileFloats shrTimeFloatsHour
                    hrDTFloatZip_3            = zip shrFileFloats shrDateWords 

                    maxHRFloatZip             = maximumBy (comparing fst) hrDTFloatZip
                    maxZip_3                  = maximumBy (comparing fst) hrDTFloatZip_3
                    max_zip3                  = zip_zip maxHRFloatZip maxZip_3

                    minHRFloatZip             = minimumBy (comparing fst) hrDTFloatZip
                    minZip_3                  = minimumBy (comparing fst) hrDTFloatZip_3
                    min_zip3                  = zip_zip minHRFloatZip minZip_3

                    maxHRShowFloatZip         = hrShow3 max_zip3
                    minHRShowFloatZip         = hrShow3 min_zip3

                    hrFloatZipStats           = ("\n" ++ "Global Heart Rate Stats: "  ++
                                                 "\n" ++ "\n" ++
                                                 "Max " ++ maxHRShowFloatZip ++ "\n" ++ "\n" ++
                                                 "Min " ++ minHRShowFloatZip ++ "\n"
                                                 )
                                            
                putStrLn hrFloatZipStats      
                                 --END Global Max Min Function--
        
        else do putStr ("\n"++ "ERROR: Heart Rate folder is missing. Please return folder to Memory program:" ++ "\n")


--
--WORD SORT FUNCTION
--
{-wordSort is an inside Function. Once wordSort has been performed, 
a search can be done to find that words placement in the wordSort. 
This is a temp file that re-Writes each call to wordSort. -}
internalhelp _ = do
     clearScreen
     setCursorPosition 1 0
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStrLn ("\n" ++ "INTERNAL HELP MENU:"++"\n"++"\n")
     setSGR [ SetUnderlining SingleUnderline ]      
     putStrLn "INTERNAL MEMORY COMMANDS:"
     setSGR [Reset]                    
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 "   wordsort" 
     setSGR [Reset]
     putStr                 ("   to return Top 100 most commonly used words in MEMORY       |" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 "   searchWS" 
     setSGR [Reset]
     putStr                 ("   to search for a word's position in Word Frequency List     |" ++ "\n")
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr ("\n" ++ "Enter a Command from ")
     setSGR [ SetUnderlining SingleUnderline ] 
     putStr "MEMORY COMMANDS"
     setSGR [ SetUnderlining NoUnderline ]
     putStr  (" then follow proceeding prompt:" ++ "\n")
     setSGR [Reset]

wordsort _ = do
    clearScreen
    setCursorPosition 1 0
    contents     <- readFile "workingMemory.txt"
    let wordsContents       = words contents
        qSortWC             = quicksort wordsContents
        groupQSWC           = group qSortWC
        sortLengthGQ        = reverse $ sortWith length groupQSWC
        mapLengthSLGQ       = map length sortLengthGQ
        mapGQ               = map head sortLengthGQ
        zipMLGQ             = zip mapGQ mapLengthSLGQ
        numTop100           = zipWith3 (\n line m -> show n ++ " - " ++ line ++ "(= " ++ show m ++ ")") 
                              [0..100] mapGQ mapLengthSLGQ 
        numTopTotal         = zipWith3 (\n line m -> show n ++ " - " ++ line ++ "(= " ++ show m ++ ")") 
                              [0..] mapGQ mapLengthSLGQ  

    writeFile "tempWordSort.txt" (unlines numTopTotal)
    setSGR [ SetConsoleIntensity BoldIntensity ]    
    putStrLn ("\n" ++ "MEMORY: Word Frequency")
    setSGR [Reset]
    putStrLn ("Top 100 words in MEMORY: " ++ "\n")
    setSGR [ SetColor Foreground Vivid Blue ] 
    putStrLn $ unlines numTop100
    setSGR [ Reset ]
    putStr ("\n" ++ "Press any key to clear screen and return to Main Menu:")
    key <- getLine
    clearScreen
    setCursorPosition 1 0
   
{-SearchWS will search for a word in tempWordSort.txt -a file
  re-written each time wordsort is called. -} 
searchWS _ = do
    clearScreen
    setCursorPosition 1 0
    contents    <-readFile "tempWordSort.txt"
    putStr      "Enter item to search for in WordSort Frequency File : " 
    searchword  <- getLine

    let lowerTempWordSort  = map toLower contents
        listLTWS           = lines lowerTempWordSort 
        lowerSearchWord    = map toLower searchword

        searchListTempPosition  = mFilter (isSearchWordInWordSort lowerSearchWord) listLTWS

        unlinesSLTP        = unlines searchListTempPosition

    putStr ("\n" ++ "Positon of ")
    setSGR [ SetConsoleIntensity BoldIntensity ]
    putStr searchword 
    setSGR [Reset]
    putStrLn (" in WordSort Memory is: ")
    setSGR [ SetColor Foreground Vivid Blue ]
    putStrLn ( "\n" ++ unlinesSLTP)
    setSGR [Reset]
    putStr   ("\n" ++ "Press any key to clear screen and return to Main Menu:")
    key <- getLine
    clearScreen
    setCursorPosition 1 0 

























