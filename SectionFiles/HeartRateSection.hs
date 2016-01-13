module HeartRateSection where 

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



heartRate_help _ = do
     setScreen
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
     setScreen
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
     setScreen
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
     setScreen
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

