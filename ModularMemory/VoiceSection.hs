module VoiceSection where 

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
