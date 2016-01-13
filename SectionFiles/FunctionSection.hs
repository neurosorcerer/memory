module FunctionSection where 


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