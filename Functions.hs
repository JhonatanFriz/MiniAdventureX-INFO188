module Functions (
    Coordinate(..),
    newSeed,
    createBoard,
    replaceStringAtIndex,
    replaceCharAtIndex,
    placePlayer,
    placeTreasure,
    printBoard,
    nextTurn,
    generateBoard,
    move,
    createLavaPools,
    createLavaPool,
    createLavaRow,
    createWalls,
    createWall,
    randomCoordinateWithSeedAndLimit,
    randomWallLength,
    randomOrientation,
    randomLava,
    randomFirstRowLength,
    randomNumberOfPools,
    randomNumberOfWalls,
)
where

import System.Random
import System.IO

data Coordinate = Coordinate {x :: Int, y :: Int} deriving (Show)

newSeed :: Int -> Int
newSeed s = s+1

-- Crea una lista de n listas de tamaño n, donde cada elemento es un espacio en blanco
createBoard :: Int -> [String]
createBoard n = replicate n (replicate n ' ')

-- Reemplaza un caracter de un string en una lista de strings dicho en un indice
replaceStringAtIndex :: Int -> Int -> Char -> [String] -> [String]
replaceStringAtIndex _ _ _ [] = []
replaceStringAtIndex index index2 newChar list
    | index < 0 || index >= length list = list -- cuando el indice sale fuera de la lista de strings queda igual
    | otherwise =
        let (before, stringAtIndex : after) = splitAt index list
        in
            case replaceCharAtIndex index2 newChar stringAtIndex of
                Nothing -> list  -- no reemplaza si el indice sale de los limites de stringAtIndex
                Just modifiedString -> before ++ modifiedString : after

-- Reemplaza un caracter de un string en el indice indicado
replaceCharAtIndex :: Int -> Char -> String -> Maybe String
replaceCharAtIndex index newChar str
    | index < 0 || index >= length str = Nothing -- cuando el indice sale fuera del del string no hay cambio
    | otherwise = Just $ take index str ++ [newChar] ++ drop (index + 1) str

-- Funciones para establecer la posición del jugador y del tesoro
placePlayer :: Coordinate -> [String] -> [String]
placePlayer c t = replaceStringAtIndex (x c) (y c) '@' t

placeTreasure :: Coordinate -> [String] -> [String]
placeTreasure c t = replaceStringAtIndex (x c) (y c) 'X' t

-- Imprime el tablero en la terminal
printBoard :: [String] -> IO ()
printBoard board = do
  let width = length board
  putStrLn ("╔" ++ replicate (width*2+1) '═'  ++ "╗")
  mapM_ (\s -> putStrLn $ "║ " ++ unwords (map return s) ++ replicate (width - length s - 4) ' ' ++ " ║") board
  putStrLn ("╚" ++ replicate (width*2+1) '═'  ++ "╝")

-- Función principal del juego que recibe una acción y realiza lo que corresponda
nextTurn :: [String] -> Int -> Coordinate -> Coordinate -> IO ()
nextTurn board s p t = do
    let n = length board
    -- Recibe la acción del jugador
    putStrLn "Ingrese una acción de movimiento:"
    hFlush stdout
    input <- getLine
    let mov = head input
    putStrLn $ "Su movimiento es: " ++ show mov
    putStrLn $ "Lo que paso es: " ++ checkInput mov
    -- Si el jugador elige regenerar el tablero
    if mov == 'R'
        then do
            let newBoard = generateBoard n (s+253) p t
            printBoard newBoard
            nextTurn newBoard (s+65) p t
        else do
        let p' = move p mov
        -- Si la posición siguiente del jugador está dentro del tablero
        if (x p') < n && (y p') < n && (x p') >= 0 && (y p') >= 0
            then do
            let cell = (board !! (x p')) !! (y p')
            -- Si la casilla a la que se moverá el jugador es caminable, hay lava o está el tesoro
            if cell == ' ' || cell == '$' || cell == 'X'
                then do
                    -- Actualiza el tablero y se imprime en la terminal
                    let board' = replaceStringAtIndex (x p) (y p) ' ' board
                    let board'' = replaceStringAtIndex (x p') (y p') '@' board'
                    printBoard board''
                    -- Si la casilla es caminable continúa al siguiente turno
                    if cell == ' '
                        then do
                            nextTurn board'' (s+66) p' t
                        -- Si no, se termina el juego imprimiendo lo que corresponde
                        else if cell == '$'
                            then do
                                putStrLn "Game Over"
                            else do
                                putStrLn "You Win!"
                else do
                    printBoard board
                    nextTurn board (s+47) p t
            else do
                printBoard board
                nextTurn board (s+52) p t

-- Imprime en la terminal la acción recibida
checkInput :: Char -> String
checkInput n
    | n == 'W' = "Movido hacia arriba."
    | n == 'A' = "Movido hacia la izquierda."
    | n == 'S' = "Movido hacia abajo."
    | n == 'D' = "Movido hacia la derecha."
    | n == 'R' = "El mapa ha sido regenerado."
    | otherwise = "Use W, A, S, D o R."

-- Genera el tablero
generateBoard :: Int -> Int -> Coordinate -> Coordinate -> [String]
generateBoard n s p t = do
    let board = createBoard n
    let board' = placePlayer p board
    let board'' = placeTreasure t board'
    let numberOfPools = randomNumberOfPools n (s+444)
    let board''' = createLavaPools board'' (s+32) numberOfPools
    let numberOfWalls = randomNumberOfWalls n (s+445)
    let board'''' = createWalls board''' s numberOfWalls
    board''''

-- Genera la cantidad de piscinas de lava que recibe como parámetro
createLavaPools :: [String] -> Int -> Int -> [String]
createLavaPools board s remainingPools
    | remainingPools > 0 = do
        let n = length board
        let c = randomCoordinateWithSeedAndLimit n (s+99)
        let x1 = x c
        let y1 = y c
        let firstRowLength = randomFirstRowLength (s+21)
        let y2 = y1 + firstRowLength
        let poolLength = firstRowLength --rowLength*2
        let board' = createLavaPool board x1 y1 y2 n (s+50) poolLength
        createLavaPools board' (s+13) (remainingPools-1)
    | otherwise = board

-- Genera una piscina de lava
createLavaPool :: [String] -> Int -> Int -> Int -> Int -> Int -> Int -> [String]
createLavaPool board x y1 y2 n s remainingRows =
    if remainingRows > 0 && y1 < y2
        then do
            let y1' = y1 + randomLava (s+11)
            let y2' = y2 + randomLava (s+12)
            let board' = createLavaRow board x y1 y2
            -- Si la fila empieza antes de la posición 0
            if y1' < 0
                then do
                    createLavaPool board' (x+1) 0 y2' n (s+80) (remainingRows-1)
                else do
                    createLavaPool board' (x+1) y1' y2' n (s+80) (remainingRows-1)
        else board

-- Genera una fila de una piscina de lava
createLavaRow :: [String] -> Int -> Int -> Int -> [String]
createLavaRow board x y1 y2 = do
    let n = length board
    if x < n && y1 < n && y1 <= y2 && (board !! x) !! y1 == ' '
        then do
            let board' = replaceStringAtIndex x y1 '$' board
            createLavaRow board' x (y1+1) y2
    else board

-- Retorna la coordenada a la que se moverá el jugador
move :: Coordinate -> Char -> Coordinate
move p mov
    | mov == 'W' = Coordinate (x p - 1) (y p) --(fst p - 1, snd p)
    | mov == 'A' = Coordinate (x p) (y p - 1) --(fst p, snd p - 1)
    | mov == 'S' = Coordinate (x p + 1) (y p) --(fst p + 1, snd p)
    | mov == 'D' = Coordinate (x p) (y p + 1) --(fst p, snd p + 1)
    | otherwise = p

-- Genera la cantidad de murallas que recibe como parámetro
createWalls :: [String] -> Int -> Int -> [String]
createWalls board s limit =
    if limit > 0
        then do
            let n = length board
            let c = randomCoordinateWithSeedAndLimit n (s+2)
            let l = randomWallLength n (s+3)
            let o = randomOrientation (s+4)
            let board' = createWall board (x c) (y c) n l o
            createWalls board' (s+30) (limit-1)
        else board

-- Función recursiva que genera un muro de longitud l, a partir de las coordenadas x e y
createWall :: [String] -> Int -> Int -> Int -> Int -> Bool -> [String]
createWall board x y n l isVertical =
    if l > 0 && x < n && y < n && (board !! x) !! y == ' '
        then do
            let board' = replaceStringAtIndex x y 'L' board
            if isVertical
            then createWall board' (x+1) y n (l-1) isVertical
            else createWall board' x (y+1) n (l-1) isVertical
    else board

-- Retorna una coordenada aleatoria (fila, columna) para un tablero de n*n a partir de una semilla s
randomCoordinateWithSeedAndLimit :: Int -> Int -> Coordinate
randomCoordinateWithSeedAndLimit n s = let
    gen = mkStdGen s
    m = n - 1
    (_, newGen) = split gen
    (x, gen') = randomR (0, m) newGen
    (y, gen'') = randomR (0, m) gen'
    in (Coordinate x y)

-- Retorna un número aleatorio que determina el largo de una muralla
randomWallLength :: Int -> Int -> Int
randomWallLength n s =
    let (l, _) = randomR (2,n-1) (mkStdGen s)
    in l

randomLava :: Int -> Int
randomLava s =
    let (l, _) = randomR (-1,1) (mkStdGen s)
    in l

-- Retorna un número aleatorio que determina el largo de la primera fila de una piscina de lava
randomFirstRowLength :: Int -> Int
randomFirstRowLength s =
    let (l, _) = randomR (2,4) (mkStdGen s)
    in l

-- Retorna verdadero para determinar una orientación vertical de la muralla y falso para horizontal
randomOrientation :: Int -> Bool
randomOrientation s =
    let (o, _) = randomR (0,1) (mkStdGen s)
    in (o == (1 :: Int))

-- Retorna un número aleatorio que determina la cantidad de piscinas de lava a generar
randomNumberOfPools :: Int -> Int -> Int
randomNumberOfPools n s =
    let (limit, _) = randomR (2,5) (mkStdGen s)
    in limit

-- Retorna un número aleatorio que determina la cantidad de murallas a generar
randomNumberOfWalls :: Int -> Int -> Int
randomNumberOfWalls n s =
    let (limit, _) = randomR (3,n-3) (mkStdGen s)
    in limit