--MAIN

import DispatchSection 
import MemorySection
import HeartRateSection
import VoiceSection
import WordSortSection



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
     setScreen  

help _ = do
     setScreen
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
     setScreen
     putStrLn $ "Sorry Dr. Worrell, the "++command++" command doesn't exist."

--
