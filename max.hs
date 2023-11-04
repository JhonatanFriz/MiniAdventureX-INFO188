{-# LANGUAGE FlexibleInstances #-}
import System.Environment
import System.Random
import System.IO
import Data.Typeable

-- funcion que crea una lista de listas, donde cada caracter de es un espacio en blanco
createTab :: Int -> [[Char]]
createTab n = replicate n (replicate n ' ')

-- formarTablero :: [[Char]] -> [[Char]]

-- ubicarJugador :: (Int, Int) -> [[Char]] -> [[Char]]
-- for i in filas 
-- for j in columnas
-- if (i,j)=(x,y) then @

-- ubicarTesoro :: (Int, Int) -> [[Char]] -> [[Char]]
-- for i in filas 
-- for j in columnas
-- if (i,j)=(x,y) then X

randomCoordinate :: Int -> IO (Int, Int)
randomCoordinate n = do
    let m = n - 1
    x <- randomRIO (0, m)
    y <- randomRIO (0, m)
    return (x, y)


showMatrix :: [[Char]] -> IO ()
showMatrix matrix = do
    mapM_ (putStrLn . concatMap (\cell -> "[" ++ [cell]++ "]")) matrix

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
        then putStrLn "Uso: ./max <n> <s>"
    else do
        let n = read (args !! 0) :: Int
            s = read (args !! 1) :: Int
        putStrLn ("n = " ++ show n ++ ", s = " ++ show s)
        let tablero = createTab n
        showMatrix tablero
        movement
        let coordinatePlayer = randomCoordinateWithSeedAndLimit n s
        
        let coordinateTreasure = (n-1,n-1) -- randomCoordinateWithSeedAndLimit n s
        -- let coordinateTreasure = whileCoordinatesDiffer coordinatePlayer coordinateTreasure n s
        -- let positionsList = [(coordinatePlayer, coordinateTreasure)]
        putStrLn $ "Coordenada Jugador: " ++ show coordinatePlayer
        putStrLn $ "Coordenada Tesoro: " ++ show coordinateTreasure
        -- showTup coordinateString
        -- let player = setPlayer s


-- instance Show ((Int, Int), String) where
--    show ((x, y), str) = "(" ++ show x ++ ", " ++ show y ++ ")"

showTup :: (Show a, Show b) => (a,b) -> String
showTup (a,b) = (show a) ++ "," ++ (show b)

-- funciones para probar inputs de usuario
movement :: IO ()
movement = do
    putStrLn "Ingrese una accion de movimiento (entre comillas):"
    input <- getLine
    let mov = read input :: String
    putStrLn $ "Su movimiento es: " ++ show mov
    putStrLn $ "Lo que paso es: " ++ checkInput mov

checkInput :: String -> String
checkInput n
    | n == "W" = "Movido hacia arriba." --(x,y-1)
    | n == "A" = "Movido hacia la izquierda." --(x-1,y)
    | n == "S" = "Movido hacia abajo." --(x,y+1)
    | n == "D" = "Movido hacia la derecha." --(x+1,y)
    | n == "R" = "El mapa ha sido regenerado."
    | otherwise = "Use W, A, S, D o R."

-- setPlayer :: Int -> [Int]
-- setPlayer s = do
    -- Generar x entre 0 y n-1
    -- Generar y entre 0 y n-1
    
randomCoordinateWithSeedAndLimit :: Int -> Int -> (Int, Int)
randomCoordinateWithSeedAndLimit n s = let
    gen = mkStdGen s
    m = n - 1
    (_, newGen) = split gen
    (x, gen') = randomR (0, m) newGen
    (y, gen'') = randomR (0, m) gen'
    in (x, y)

-- printMatrix :: [[a]] -> IO ()
-- printMatrix matrix = mapM_ (\row -> do
--   mapM_ (\element -> putStr (show element ++ " ")) row
--  putStrLn "") matrix

whileCoordinatesDiffer :: (Int, Int) -> (Int, Int) -> Int -> Int -> (Int, Int)
whileCoordinatesDiffer coord1 coord2 n s =
    if coord1 /= coord2
        then coord2
        else do
            let coord3 = randomCoordinateWithSeedAndLimit n s
            whileCoordinatesDiffer coord1 coord3 n s
