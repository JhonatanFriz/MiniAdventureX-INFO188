module Functions (
    createBoard,
    generateWall,
    replaceStringAtIndex,
    replaceCharAtIndex,
    placePlayer,
    placeTreasure,
    randomCoordinate,
    showMatrix,
    printBoard,
    game,
    generateBoard,
    move,
    createLavaPools,
    createLavaPool,
    createLavaRow,
    generateLava,
    createWalls,
    createWall,
    randomCoordinateWithSeedAndLimit,
    randomLength,
    randomOrientation,
    randomLava,
    randomLavaLength,
    randomInitialLava,
    randomLavaLimit,
    randomWallsLimit,
)
where

import System.Random
import System.IO

-- Crea una lista de listas (n*n), donde cada elemento es un espacio en blanco
createBoard :: Int -> [String]
createBoard n = replicate n (replicate n ' ')

generateWall :: [String] -> Int -> Int -> Int -> Int -> Bool -> [String]
generateWall board x y n l isVertical =
    if l > 0 && x < n && y < n && (board !! x) !! y == ' '
        then do
            let board' = replaceStringAtIndex x y 'L' board
            if isVertical
            then generateWall board' (x+1) y n (l-1) isVertical
            else generateWall board' x (y+1) n (l-1) isVertical
    else board

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

placePlayer :: (Int, Int) -> [String] -> [String]
placePlayer (x, y) t = replaceStringAtIndex x y '@' t

placeTreasure :: (Int, Int) -> [String] -> [String]
placeTreasure (x, y) t = replaceStringAtIndex x y 'X' t

-- Genera una coordenada random (x, y)
randomCoordinate :: Int -> IO (Int, Int)
randomCoordinate n = do
    let m = n - 1
    x <- randomRIO (0, m)
    y <- randomRIO (0, m)
    return (x, y)

-- Imprime el tablero en la terminal
showMatrix :: [[Char]] -> IO ()
showMatrix matrix = do
    mapM_ (putStrLn . concatMap (\cell -> "[" ++ [cell]++ "]")) matrix

-- Imprime el tablero en la terminal
printBoard :: [String] -> IO ()
printBoard board = do
  let width = length board
  putStrLn ("╔" ++ replicate (width*2+1) '═'  ++ "╗")
  mapM_ (\s -> putStrLn $ "║ " ++ unwords (map return s) ++ replicate (width - length s - 4) ' ' ++ " ║") board
  putStrLn ("╚" ++ replicate (width*2+1) '═'  ++ "╝")

-- Función principal del juego que recibe una acción y realiza lo que corresponda
game :: [String] -> Int -> (Int, Int) -> (Int, Int) -> IO ()
game board s p t = do
    putStrLn "Ingrese una acción de movimiento:"
    hFlush stdout
    input <- getLine
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
        let p' = move p mov
        if fst p' < n && snd p' < n && fst p' >= 0 && snd p' >= 0
            then do
            let cell = (board !! fst p') !! snd p'
            putStrLn ("cell: " ++ show cell)
            if cell == ' ' || cell == '$' || cell == 'X'
                then do
                    let board' = uncurry replaceStringAtIndex p ' ' board
                    let board'' = uncurry replaceStringAtIndex p' '@' board'
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

checkInput :: Char -> String
checkInput n
    | n == 'W' = "Movido hacia arriba." --(x,y-1)
    | n == 'A' = "Movido hacia la izquierda." --(x-1,y)
    | n == 'S' = "Movido hacia abajo." --(x,y+1)
    | n == 'D' = "Movido hacia la derecha." --(x+1,y)
    | n == 'R' = "El mapa ha sido regenerado."
    | otherwise = "Use W, A, S, D o R."

-- Genera el tablero
generateBoard :: Int -> Int -> (Int, Int) -> (Int, Int) -> [String]
generateBoard n s p t = do
    let board = createBoard n
    let board' = placePlayer p board
    let board'' = placeTreasure t board'
    let lavaLimit = 4 --randomLavaLength n (s+444)
    let board''' = createLavaPools board'' n (s+32) lavaLimit
    let wallsLimit = randomWallsLimit n (s+445)
    let board'''' = createWalls board''' n s wallsLimit
    board''''

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
    l = randomLavaLength n (s+3)
    in generateLava board x y1 y2 n

generateLava :: [String] -> Int -> Int -> Int -> Int -> [String]
generateLava board x y1 y2 n =
    if x < n && y1 < n && y1 <= y2 && (board !! x) !! y1 == ' '
        then do
            let board' = replaceStringAtIndex x y1 '$' board
            generateLava board' x (y1+1) y2 n
    else board

-- Retorna la coordenada a la que se moverá el jugador
move :: (Int, Int) -> Char -> (Int, Int)
move p mov
    | mov == 'W' = (fst p - 1, snd p)
    | mov == 'A' = (fst p, snd p - 1)
    | mov == 'S' = (fst p + 1, snd p)
    | mov == 'D' = (fst p, snd p + 1)
    | otherwise = p

createWalls :: [String] -> Int -> Int -> Int -> [String]
createWalls board n s limit =
    if limit > 0
        then do
            let board' = createWall board n s
            createWalls board' n (s+30) (limit-1)
        else board

createWall :: [String] -> Int -> Int -> [String]
createWall board n s = let
    (x, y) = randomCoordinateWithSeedAndLimit n (s+2)
    l = randomLength n (s+3)
    o = randomOrientation (s+4)
    in generateWall board x y n l o

-- Funciones random

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
    let (l, _) = randomR (2,5) (mkStdGen s)
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

randomLavaLimit :: Int -> Int -> Int
randomLavaLimit n s =
    let (limit, _) = randomR (2,5) (mkStdGen s)
    in limit

randomWallsLimit :: Int -> Int -> Int
randomWallsLimit n s =
    let (limit, _) = randomR (3,n-3) (mkStdGen s)
    in limit