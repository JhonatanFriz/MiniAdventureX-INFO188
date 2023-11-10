{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import System.Environment
--import System.Random
--import System.IO
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
        putStrLn ("n = " ++ show n ++ ", s = " ++ show s)
        let tablero = createBoard n
        let coordinatePlayer = randomCoordinateWithSeedAndLimit n s
        
        let coordinateTreasure = randomCoordinateWithSeedAndLimit n (s+1)
        -- let coordinateTreasure = whileCoordinatesDiffer coordinatePlayer coordinateTreasure n s

        putStrLn $ "Coordenada Jugador: " ++ show coordinatePlayer
        putStrLn $ "Coordenada Tesoro: " ++ show coordinateTreasure

        let tablero1 = placePlayer coordinatePlayer tablero
        let tablero2 = placeTreasure coordinateTreasure tablero1

        
        putStrLn "Tablero con lava:"
        let tablero3 = createLavaPools tablero2 n (s+32) 5
        showMatrix tablero3
        putStrLn "Tablero con lava y murallas:"
        let numberOfWalls = randomNumberOfWalls n s
        let tablero4 = createWalls tablero3 n s numberOfWalls
        showMatrix tablero4

        game tablero4 (s+21) coordinatePlayer coordinateTreasure
