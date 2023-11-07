{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import System.Environment
import System.Random
import System.IO
import Data.List
import Data.Typeable
import GHC.Unit.Home.ModInfo (listHMIToHpt)
import Data.Array.Base (bOOL_BIT)

-- funcion que crea una lista de listas, donde cada caracter de es un espacio en blanco
createTab :: Int -> [String]
createTab n = replicate n (replicate n ' ')

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
--elemento :: [String] -> Int -> Int -> Int -> Bool -> Char
--elemento tablero x y l o = (tablero !! x) !! y

showXY :: Int -> Int -> IO ()
showXY x y = putStrLn ("x: " ++ show x ++ ", y: " ++ show y)

elemento :: [String] -> Int -> Int -> Int -> Int -> Bool -> [String]
elemento tablero x y n l isVertical =
    if l > 0 && x < n && y < n && (tablero !! x) !! y == ' '
        then do
            let tablero' = replaceStringAtIndex x y 'L' tablero
            if isVertical
            then elemento tablero' (x+1) y n (l-1) isVertical
            else elemento tablero' x (y+1) n (l-1) isVertical
    else tablero
    
-- if (tablero !! x) !! y == 'a'
    -- reemplazar x y por 'L'
    -- elemento tablero x (y+1) (l-1) o --¿algo así?

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
--  | index == (length str - 1) = Just $ take index str ++ [newChar]
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

-- printBoard :: [String] -> IO ()
-- printBoard board = do
    --let n = length board
    --putStrLn ("a " ++ concat ["b " | r <- [0..n]] ++ "c")
    --mapM_ (putStrLn . concatMap (: " ")) board
    --mapM_ (putStrLn . (\x -> "a" ++ x ++ "c")) board
    --mapM_ (putStrLn . (\z -> "a" ++ (putStrLn . unwords . map (\x -> x ++ " ")))) board
    --putStrLn ("a " ++ concat ["b " | r <- [0..n]] ++ "c")

-- imprimirMatrizConAB :: [String] -> IO ()
-- imprimirMatrizConAB [] = return ()
-- imprimirMatrizConAB (fila:filas) = do
--   putStrLn ("a" ++ unwords (map (++ " ") (words fila)) ++ "b")
--   imprimirMatrizConAB filas

printBoard :: [String] -> IO ()
printBoard board = do
  let width = length board
  putStrLn ("╔" ++ replicate (width*2+1) '═'  ++ "╗")
  mapM_ (\s -> putStrLn $ "║ " ++ unwords (map return s) ++ replicate (width - length s - 4) ' ' ++ " ║") board
  putStrLn ("╚" ++ replicate (width*2+1) '═'  ++ "╝")

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
        then putStrLn "Uso: ./max <n> <s>"
    else do
        let n = read (head args) :: Int
            s = read (args !! 1) :: Int
        putStrLn ("n = " ++ show n ++ ", s = " ++ show s)
        let tablero = createTab n
        --showMatrix tablero
        --movement
        let coordinatePlayer = randomCoordinateWithSeedAndLimit n s
        
        let coordinateTreasure = randomCoordinateWithSeedAndLimit n (s+1)
        -- let coordinateTreasure = whileCoordinatesDiffer coordinatePlayer coordinateTreasure n s

        putStrLn $ "Coordenada Jugador: " ++ show coordinatePlayer
        putStrLn $ "Coordenada Tesoro: " ++ show coordinateTreasure

        -- tableroIO <- convertToIOList tablero
        let tablero1 = ubicarJugador coordinatePlayer tablero
        let tablero2 = ubicarTesoro coordinateTreasure tablero1
        -- fila x columna
        --showMatrix tablero2

        -- Creación de murallas
        --let x = 0
        --let y = 6
        --let (x, y) = randomCoordinateWithSeedAndLimit n (s+2)
        --let l = randomLength n (s+3)
        --let o = randomOrientation (s+4)
        --check x y l o
        
        --let tablero3 = elemento tablero2 x y n l o

        let limit = randomLimit n s
        --let tablero3 = createWalls tablero2 n s limit
        --showMatrix tablero3
        --putStrLn "Tablero con lava:"
        --let tablero4 = createLavaPool tablero3 5 7 n (s+50) 7
        --showMatrix tablero4
        putStrLn "Tablero con lava:"
        -- let (x, y1) = randomCoordinateWithSeedAndLimit n (s+99)
        -- let il = randomInitialLava (s+21)
        -- let y2 = y1 + il
        -- let ll = il*2
        -- showXY x y1
        -- let tablero3 = createLavaPool tablero2 x y1 y2 n (s+50) ll
        let tablero3 = createLavaPools tablero2 n (s+32) 5
        showMatrix tablero3
        putStrLn "Tablero con lava y murallas:"
        let tablero4 = createWalls tablero3 n s limit
        showMatrix tablero4

        --printBoard tablero4

        game tablero4 (s+21) coordinatePlayer coordinateTreasure

game :: [String] -> Int -> (Int, Int) -> (Int, Int) -> IO ()
game board s p t = do
    putStrLn "Ingrese una acción de movimiento:"
    hFlush stdout
    input <- getLine
    --let ip = read input :: String
    let mov = head input
    putStrLn $ "Su movimiento es: " ++ show mov
    putStrLn $ "Lo que paso es: " ++ checkInput mov
    let n = length board
    if mov == 'R'
        then do
            let newBoard = generateBoard n (s+253) p t
            showMatrix newBoard
            game newBoard (s+65) p t
        else do
        let p' = move board p mov
        if fst p' < n && snd p' < n && fst p' >= 0 && snd p' >= 0
            then do
            let cell = (board !! fst p') !! snd p'
            putStrLn ("cell: " ++ show cell)
            if cell == ' ' || cell == '$' || cell == 'X'
                then do
                    let board' = replaceStringAtIndex (fst p) (snd p) ' ' board
                    let board'' = replaceStringAtIndex (fst p') (snd p') '@' board'
                    showMatrix board''
                    if cell == ' '
                        then do
                            game board'' (s+66) p' t
                        else if cell == '$'
                            then do
                                putStrLn "Game Over"
                            else do
                                putStrLn "Wiii"
                else do
                    showMatrix board
                    game board (s+47) p t
            else do
                showMatrix board
                game board (s+52) p t

move :: [String] -> (Int, Int) -> Char -> (Int, Int)
move board p mov
    | mov == 'W' = (fst p - 1, snd p)
    | mov == 'A' = (fst p, snd p - 1)
    | mov == 'S' = (fst p + 1, snd p)
    | mov == 'D' = (fst p, snd p + 1)
    | otherwise = p

-- funciones para probar inputs de usuario
movement :: IO ()
movement = do
    putStrLn "Ingrese una accion de movimiento:"
    input <- getLine
    let mov = read input :: String
    putStrLn $ "Su movimiento es: " ++ show mov
    --putStrLn $ "Lo que paso es: " ++ checkInput mov

checkInput :: Char -> String
checkInput n
    | n == 'W' = "Movido hacia arriba." --(x,y-1)
    | n == 'A' = "Movido hacia la izquierda." --(x-1,y)
    | n == 'S' = "Movido hacia abajo." --(x,y+1)
    | n == 'D' = "Movido hacia la derecha." --(x+1,y)
    | n == 'R' = "El mapa ha sido regenerado."
    | otherwise = "Use W, A, S, D o R."

randomCoordinateWithSeedAndLimit :: Int -> Int -> (Int, Int)
randomCoordinateWithSeedAndLimit n s = let
    gen = mkStdGen s
    m = n - 1
    (_, newGen) = split gen
    (x, gen') = randomR (0, m) newGen
    (y, gen'') = randomR (0, m) gen'
    in (x, y)

randomLength :: Int -> Int -> Int
randomLength n s =
    let (l, _) = randomR (2,n-1) (mkStdGen s)
    in l

randomLavaLength :: Int -> Int -> Int
randomLavaLength n s =
    let (l, _) = randomR (2,7) (mkStdGen s)
    in l

randomLava :: Int -> Int
randomLava s =
    let (l, _) = randomR (-1,1) (mkStdGen s)
    in l

randomInitialLava :: Int -> Int
randomInitialLava s =
    let (l, _) = randomR (2,4) (mkStdGen s)
    in l

randomOrientation :: Int -> Bool
randomOrientation s =
    let (o, _) = randomR (0,1) (mkStdGen s)
    in (o == (1 :: Int))

randomLimit :: Int -> Int -> Int
randomLimit n s =
    let (limit, _) = randomR (3,n-3) (mkStdGen s)
    in limit

whileCoordinatesDiffer :: (Int, Int) -> (Int, Int) -> Int -> Int -> (Int, Int)
whileCoordinatesDiffer coord1 coord2 n s =
    if coord1 /= coord2
        then coord2
        else do
            let coord3 = randomCoordinateWithSeedAndLimit n s
            whileCoordinatesDiffer coord1 coord3 n s

check :: Int -> Int -> Int -> Bool -> IO ()
check x y l o =
    putStrLn ("Muralla: (" ++ show x ++ "," ++ show y ++ "). Largo: " ++ show l ++ ". Orientación: " ++ show o)

createWall :: [String] -> Int -> Int -> [String]
createWall board n s = let
    (x, y) = randomCoordinateWithSeedAndLimit n (s+2)
    l = randomLength n (s+3)
    o = randomOrientation (s+4)
    --check x y l o    
    in elemento board x y n l o

createWalls :: [String] -> Int -> Int -> Int -> [String]
createWalls board n s limit =
    if limit > 0
        then do
            let board' = createWall board n s
            createWalls board' n (s+30) (limit-1)
        else board

-- createLavaPool :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
-- createLavaPool board x y n s limit =
--     if limit > 0
--         then do
--             let board' = createLavaRow board x y n s
--             createLavaPool board' (x+1) y n (s+80) (limit-1)
--         else board

createLavaPools :: [String] -> Int -> Int -> Int -> [String]
createLavaPools board n s limit =
    if limit > 0
        then do
            let (x, y1) = randomCoordinateWithSeedAndLimit n (s+99)
            let il = randomInitialLava (s+21)
            let y2 = y1 + il
            let ll = il*2
            let board' = createLavaPool board x y1 y2 n (s+50) ll
            createLavaPools board' n (s+13) (limit-1)
        else board

createLavaPool :: [String] -> Int -> Int -> Int -> Int -> Int -> Int -> [String]
createLavaPool board x y1 y2 n s limit =
    if limit > 0 && y1 <= y2
        then do
            let y1' = y1 + randomLava (s+11)
            let y2' = y2 + randomLava (s+12)
            let board' = createLavaRow board x y1 y2 n s
            if y1' < 0
                then do
                    createLavaPool board' (x+1) 0 y2' n (s+80) (limit-1)
                else do
                    createLavaPool board' (x+1) y1' y2' n (s+80) (limit-1)
        else board

createLavaRow :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
createLavaRow board x y1 y2 n s = let
    --(x, y) = randomCoordinateWithSeedAndLimit n (s+2)
    l = randomLavaLength n (s+3)
    in generateLava board x y1 y2 n

-- createLavaRow :: [String] -> Int -> Int -> Int -> Int -> [String]
-- createLavaRow board x y n s = let
--     --(x, y) = randomCoordinateWithSeedAndLimit n (s+2)
--     l = randomLavaLength n (s+3)
--     in generateLava board x y n l

generateLava :: [String] -> Int -> Int -> Int -> Int -> [String]
generateLava board x y1 y2 n =
    if x < n && y1 < n && y1 <= y2 && (board !! x) !! y1 == ' '
        then do
            let board' = replaceStringAtIndex x y1 '$' board
            generateLava board' x (y1+1) y2 n
    else board

generateBoard :: Int -> Int -> (Int, Int) -> (Int, Int) -> [String]
generateBoard n s p t = do
    let board = createTab n
    let board' = ubicarJugador p board
    let board'' = ubicarTesoro t board'
    let lavaLimit = randomLavaLength n (s+444)
    let board''' = createLavaPools board'' n (s+32) lavaLimit
    let wallsLimit = randomLength n (s+445)
    let board'''' = createWalls board''' n s wallsLimit
    board''''