{-# LANGUAGE FlexibleInstances #-}
import System.Environment
import System.Random
import System.IO
import Data.List
import Data.Typeable

-- funcion que crea una lista de listas, donde cada caracter de es un espacio en blanco
createTab :: Int -> [String]
createTab n = replicate n (replicate n 'a')

advanceSeed :: StdGen -> StdGen
advanceSeed = snd . split

-- formarTablero :: [[Char]] -> [[Char]]
-- for each space in tablero do
-- if char == ' ' then
--    random1 para ver si forma obstaculo, if true then
--      random2 para ver si es L o $

--crearMurallas :: [[Char]] -> [[Char]]
--crearMurallas t = do
    --let number = randomRIO (0,5)
    -- for i=1, i<=number do
    --  let length = randomRIO (3,6)
    --  let xy = randomRIO (0,1) -- 0 horizontal, 1 vertical

-- Crear muralla (solo una)
elemento :: [String] -> Int -> Int -> Int -> Bool -> Char
elemento tablero x y l o = (tablero !! x) !! y
-- if (tablero !! x) !! y == 'a'
    -- reemplazar x y por 'L'
    -- elemento tablero x y+1 l-1 o ¿algo así?

--  crearMuralla :: [String] -> (Int, Int) -> Int -> Bool -> [String]
--  recorrer tablero hasta encontrar la coordenada
--      if Bool == 0 then
--          [Horizontal] reemplazar caracteres con 'L' hasta agotar el contador (Int) o encontrar '@' o 'X' o 'L' ¿o '$'?
--      else
--          [Vertical] reemplazar caracteres con 'L' hasta agotar el contador (Int) o encontrar '@' o 'X' o 'L' ¿o '$'?


replaceStringAtIndex :: Int -> Int -> Char -> [String] -> [String]
replaceStringAtIndex _ _ _ [] = []  -- Handling the case of an empty list
replaceStringAtIndex index index2 newChar list
  | index < 0 || index >= length list = list  -- Handling cases where index is out of bounds
  | otherwise =
    let (before, stringAtIndex : after) = splitAt index list
    in
      case replaceCharAtIndex index2 newChar stringAtIndex of
        Nothing -> list  -- Do not replace if index2 is out of bounds in the stringAtIndex
        Just modifiedString -> before ++ modifiedString : after

replaceCharAtIndex :: Int -> Char -> String -> Maybe String
replaceCharAtIndex index newChar str
  | index < 0 || index >= length str = Nothing  -- Handling cases where index is out of bounds
  | otherwise = Just $ take index str ++ [newChar] ++ drop (index + 1) str


ubicarJugador :: (Int, Int) -> [String] -> [String]
ubicarJugador (x, y) t = replaceStringAtIndex x y '@' t

ubicarTesoro :: (Int, Int) -> [String] -> [String]
ubicarTesoro (x, y) t = replaceStringAtIndex x y 'X' t

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
        
        let coordinateTreasure = randomCoordinateWithSeedAndLimit n (s+1)
        -- let coordinateTreasure = whileCoordinatesDiffer coordinatePlayer coordinateTreasure n s

        putStrLn $ "Coordenada Jugador: " ++ show coordinatePlayer
        putStrLn $ "Coordenada Tesoro: " ++ show coordinateTreasure

        -- tableroIO <- convertToIOList tablero
        let tablero1 = ubicarJugador coordinatePlayer tablero
        let tablero2 = ubicarTesoro coordinateTreasure tablero1

        showMatrix tablero2

        -- Creación de murallas
        let x = 4
        let y = 7
        let l = 5
        let o = True
        let char = elemento tablero2 x y l o
        putStrLn ("Carácter en posición (" ++ show x ++ "," ++ show y ++ "): " ++ [char])
        putStrLn ("Largo: " ++ show l ++ ", orientación: " ++ show o)

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

randomCoordinateWithSeedAndLimit :: Int -> Int -> (Int, Int)
randomCoordinateWithSeedAndLimit n s = let
    gen = mkStdGen s
    m = n - 1
    (_, newGen) = split gen
    (x, gen') = randomR (0, m) newGen
    (y, gen'') = randomR (0, m) gen'
    in (x, y)

whileCoordinatesDiffer :: (Int, Int) -> (Int, Int) -> Int -> Int -> (Int, Int)
whileCoordinatesDiffer coord1 coord2 n s =
    if coord1 /= coord2
        then coord2
        else do
            let coord3 = randomCoordinateWithSeedAndLimit n s
            whileCoordinatesDiffer coord1 coord3 n s