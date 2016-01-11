module Section.MemoryMap where
import System.Console.ANSI
import Data.Time
import Data.Char
import System.Directory
import Memory

--
--START MEMORYMAP SECTION
--

memory_help _ = do
     clearScreen
     setCursorPosition 1 0
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStrLn ("\n" ++ "MEMORY HELP MENU:"++"\n"++"\n")
     setSGR [ SetUnderlining SingleUnderline ]      
     putStrLn "MEMORY COMMANDS:"
     setSGR [Reset]  
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]                  
     putStr                 "   memory_add" 
     setSGR [Reset]
     putStr                 ("      to add new memory                             |" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 "   memory_view" 
     setSGR [Reset]
     putStr                 ("     to view all memories                          |" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 "   memory_search" 
     setSGR [Reset]
     putStr                 ("   to search past memories                       |" ++ "\n")
     putStr                 "|enter"
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr                 "   memory_focus" 
     setSGR [Reset]
     putStr                 ("    to refine search -or focused- memories        |" ++ "\n" ++
                             "       --restart search when focus returns no memories--" ++ "\n" ++
                            "\n"
                           )
     setSGR [ SetConsoleIntensity BoldIntensity ]
     putStr "Enter a Command from "
     setSGR [ SetUnderlining SingleUnderline ] 
     putStr "MEMORY COMMANDS"
     setSGR [ SetUnderlining NoUnderline ]
     putStr  (" then follow proceeding prompt:" ++ "\n")
     setSGR [Reset]


memory_add _  = do 
     clearScreen
     setCursorPosition 1 0
     putStr "Enter Memory (body of memory): " 
     mItem    <- getLine 
     putStr "Memory Type (general idea of memory): " 
     mType    <- getLine  
     putStr "Memory path (medium of memory transmission): "
     mPath    <- getLine  
     putStr "Memory -Comments: "
     mComment <- getLine 
     
     cTime    <- getCurrentTime

     let memoryMapList   = [mType, mPath, mComment, mItem]
         mapMemoryString = unlines memoryMapList
         (y,m,d)      = toGregorian $ utctDay cTime
     setSGR [ SetColor Foreground Vivid Blue ]
     putStrLn ("\n" ++ mapMemoryString ++ show (y,m,d) ++ "\n")
     setSGR [Reset]
     putStr ("Do you wish to save this Memory? (yes/no): ")
     saveYesNo <- getLine
     if saveYesNo == "yes"
        then do putStrLn ("\n" ++ "Memory established!" ++ "\n")
                appendFile "workingMemory.txt" (mapMemoryString ++ show (y,m,d) ++"\n")
        else do putStr "Ok. Thank You!"

memory_view _ = do
    clearScreen
    setCursorPosition 1 0
    contents     <- readFile "workingMemory.txt"
    putStrLn ("\n" ++ "Your Memories Dr. Worrell:" ++ "\n")
    setSGR [ SetColor Foreground Vivid Blue ]
    setSGR [ SetColor Background Vivid Green ]
    putStrLn contents
    setSGR [ Reset ]
    putStr ("\n" ++ "Press any key to clear screen and return to Main Menu:")
    key <- getLine
    clearScreen
    setCursorPosition 1 0

memory_search _ = do

     clearScreen
     setCursorPosition 1 0
     fileExists  <- doesFileExist "workingMemory.txt"
     if fileExists
        then do memoryLines <- readFile "workingMemory.txt"
                putStr      "Enter Memory Search Term: " 
                searchword  <- getLine
                let lowerMemory        = map toLower memoryLines
                    listMemoryLines    = lines lowerMemory
                    lowerSearchWord    = map toLower searchword
                    mMapList           = parseMemoryMap listMemoryLines
                    searchMapPosition  = mFilter (isSearchWordInMap lowerSearchWord) mMapList
                    searchListPosition = searchList searchMapPosition
                    organizeSMP        = ("\n" ++ "Your Memories Dr. Worrell:" ++ 
                                          "\n" ++ unlines searchListPosition
                                         )
                    tempMemorySMP      = unlines searchListPosition
                if searchListPosition == []
                   then putStrLn ("\n"++ "Sorry, search term returned zero results.") 
                   else do writeFile "temporaryMemory.txt" (tempMemorySMP)
                           setSGR [ SetColor Foreground Vivid Blue ]
                           setSGR [ SetColor Background Vivid Green ]
                           putStrLn organizeSMP
                           setSGR [ Reset ]
        else putStr ("\n"++ "ERROR: Memory folder is missing. Please return folder to Memory program:" ++ "\n")


memory_focus _ =  do
     tempMemoryFile <- readFile "temporaryMemory.txt"
     putStr $"Enter a refined search term to focus your memory Dr. Worrell: "
     refine <- getLine
      
     let lowerTempMemory        = map toLower tempMemoryFile
         listTempMemory         = lines lowerTempMemory
         lowerRefineWord        = map toLower refine

         mMapTempList           = parseMemoryMap listTempMemory
         searchMapTempPosition  = mFilter (isSearchWordInMap lowerRefineWord) mMapTempList
         
         searchListTempPosition = searchList searchMapTempPosition
         organizeTempSMP        = ("\n" ++ "Your Memories Dr. Worrell:" ++ 
                                   "\n" ++ unlines searchListTempPosition
                                  )

         tempFocusMemorySMP      = unlines searchListTempPosition
     if searchListTempPosition == []
        then putStrLn ("\n"++ "Sorry, search term returned zero results.") 
        else do setSGR [ SetColor Foreground Vivid Blue ]
                setSGR [ SetColor Background Vivid Green ]
                putStrLn organizeTempSMP
                setSGR [ Reset ]
                writeFile "temporaryMemory.txt" (tempFocusMemorySMP)
                putStr ("\n" ++ "Press any key to clear screen and return to Main Menu:")
                key <- getLine
                clearScreen
                setCursorPosition 1 0

