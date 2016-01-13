module WordSortSection where 

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


