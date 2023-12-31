{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import System.Environment
import Data.List
import Data.Typeable
import GHC.Unit.Home.ModInfo (listHMIToHpt)
import Data.Array.Base (bOOL_BIT)
import Functions

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
        then putStrLn "Uso: ./max <n> <s>"
    else do
        let n = read (head args) :: Int
            s = read (args !! 1) :: Int
        
        let playerCoordinate = getRandomCoordinate n s
        let treasureCoordinate = getRandomCoordinate n (s+1)

        let board = generateBoard n (s+2) playerCoordinate treasureCoordinate
        printBoard board

        nextTurn board (s+3) playerCoordinate treasureCoordinate
