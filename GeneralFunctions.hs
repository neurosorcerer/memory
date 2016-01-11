module GeneralFunctions where 


import Data.List
import System.Console.ANSI

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

setBlueCyan = do
    setSGR [ SetColor Foreground Vivid Blue ]
    setSGR [ SetColor Background Vivid Cyan ]