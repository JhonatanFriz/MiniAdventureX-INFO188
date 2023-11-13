module Functions (
    Coordinate(..),
    createBoard,
    replaceStringAtIndex,
    replaceCharAtIndex,
    placePlayer,
    placeTreasure,
    printBoard,
    nextTurn,
    generateBoard,
    getNextSquare,
    createLavaPools,
    createLavaPool,
    createLavaRow,
    createWalls,
    createWall,
    getRandomCoordinate,
    getWallLength,
    getOrientation,
    getRandomModifier,
    getFirstRowLength,
    getNumberOfPools,
    getNumberOfWalls,
)
where

import System.Random
import System.IO

data Coordinate = Coordinate {x :: Int, y :: Int} deriving (Show)

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
    if input == ("") -- if null input
        then do
            putStrLn $ "Error. No se ingresó una acción."
            nextTurn board s p t
    else do
        let action = head input
        putStrLn $ "Su movimiento es: " ++ show action
        putStrLn $ "Lo que paso es: " ++ checkInput action
        -- Si el jugador elige regenerar el tablero
        if action == 'R'
            then do
                let newBoard = generateBoard n (s+253) p t
                printBoard newBoard
                nextTurn newBoard (s+65) p t
            else do
            let p' = getNextSquare p action
            -- Si la siguiente posición del jugador está dentro del tablero
            if x p' < n && y p' < n && x p' >= 0 && y p' >= 0
                then do
                let square = (board !! (x p')) !! (y p')
                -- Si la casilla a la que se moverá el jugador es caminable, hay lava o está el tesoro
                if square == ' ' || square == '$' || square == 'X'
                    then do
                        -- Actualiza el tablero y se imprime en la terminal
                        let board' = replaceStringAtIndex (x p) (y p) ' ' board
                        let board'' = replaceStringAtIndex (x p') (y p') '@' board'
                        printBoard board''
                        -- Si la casilla es caminable continúa al siguiente turno
                        if square == ' '
                            then do
                                nextTurn board'' (s+66) p' t
                            -- Si no, se termina el juego imprimiendo lo que corresponde
                            else if square == '$'
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
    let numberOfPools = getNumberOfPools n (s+1)
    let board''' = createLavaPools board'' (s+2) numberOfPools
    let numberOfWalls = getNumberOfWalls n (s+3)
    let board'''' = createWalls board''' s numberOfWalls
    board''''

-- Genera la cantidad de piscinas de lava que recibe como parámetro
createLavaPools :: [String] -> Int -> Int -> [String]
createLavaPools board s remainingPools
    | remainingPools > 0 = do
        let n = length board
        let coordinate = getRandomCoordinate n (s+1)
        let row = x coordinate
        let y1 = y coordinate
        let firstRowLength = getFirstRowLength n (s+2)
        let y2 = y1 + firstRowLength
        let poolLength = firstRowLength --rowLength*2
        let board' = createLavaPool board row y1 y2 (s+3) poolLength
        createLavaPools board' (s+4) (remainingPools-1)
    | otherwise = board

-- Genera una piscina de lava
createLavaPool :: [String] -> Int -> Int -> Int -> Int -> Int -> [String]
createLavaPool board x y1 y2 s remainingRows = do
    let n = length board
    if remainingRows > 0 && y1 < y2
        then do
            let board' = createLavaRow board x y1 y2 n
            let y1' = y1 + getRandomModifier (s+1)
            let y2' = y2 + getRandomModifier (s+2)
            -- Si la fila empieza antes de la posición 0
            if y1' < 0
                -- Se comienza desde la posición 0
                then do
                    createLavaPool board' (x+1) 0 y2' (s+3) (remainingRows-1)
                else do
                    createLavaPool board' (x+1) y1' y2' (s+4) (remainingRows-1)
        else board

-- Genera una fila de una piscina de lava
createLavaRow :: [String] -> Int -> Int -> Int -> Int -> [String]
createLavaRow board x y1 y2 n =
    if x < n && y1 < n && y1 <= y2 && (board !! x) !! y1 == ' '
        then do
            let board' = replaceStringAtIndex x y1 '$' board
            createLavaRow board' x (y1+1) y2 n
    else board

-- Retorna la coordenada a la que se moverá el jugador
getNextSquare :: Coordinate -> Char -> Coordinate
getNextSquare p action
    | action == 'W' = Coordinate (x p - 1) (y p) --(fst p - 1, snd p)
    | action == 'A' = Coordinate (x p) (y p - 1) --(fst p, snd p - 1)
    | action == 'S' = Coordinate (x p + 1) (y p) --(fst p + 1, snd p)
    | action == 'D' = Coordinate (x p) (y p + 1) --(fst p, snd p + 1)
    | otherwise = p

-- Genera la cantidad de murallas que recibe como parámetro
createWalls :: [String] -> Int -> Int -> [String]
createWalls board s remainingWalls =
    if remainingWalls > 0
        then do
            let n = length board
            let c = getRandomCoordinate n (s+2)
            let l = getWallLength n (s+3)
            let o = getOrientation (s+4)
            let board' = createWall board (x c) (y c) n l o
            createWalls board' (s+30) (remainingWalls-1)
        else board

-- Función recursiva que genera un muro de longitud remainingSquares a partir de las coordenadas x e y
createWall :: [String] -> Int -> Int -> Int -> Int -> Bool -> [String]
createWall board x y n remainingSquares isVertical =
    if remainingSquares > 0 && x < n && y < n && (board !! x) !! y == ' '
        then do
            let board' = replaceStringAtIndex x y 'L' board
            if isVertical
            then createWall board' (x+1) y n (remainingSquares-1) isVertical
            else createWall board' x (y+1) n (remainingSquares-1) isVertical
    else board

-- Retorna una coordenada aleatoria (fila, columna) para un tablero de n*n a partir de una semilla s
getRandomCoordinate :: Int -> Int -> Coordinate
getRandomCoordinate n s = let
    gen = mkStdGen s
    m = n - 1
    (_, newGen) = split gen
    (x, gen') = randomR (0, m) newGen
    (y, gen'') = randomR (0, m) gen'
    in (Coordinate x y)

-- Retorna un número aleatorio que determina el largo de una muralla
getWallLength :: Int -> Int -> Int
getWallLength n s =
    let (l, _) = randomR (2,n-1) (mkStdGen s)
    in l

-- Retorna un número entero aleatorio entre -1 y 1
getRandomModifier :: Int -> Int
getRandomModifier s =
    let (m, _) = randomR (-1,1) (mkStdGen s)
    in m

-- Retorna un número aleatorio que determina el largo de la primera fila de una piscina de lava
getFirstRowLength :: Int -> Int -> Int
getFirstRowLength n s =
    let (l, _) = randomR (2,(div n 4)) (mkStdGen s)
    in l

-- Retorna verdadero para determinar una orientación vertical de la muralla y falso para horizontal
getOrientation :: Int -> Bool
getOrientation s =
    let (o, _) = randomR (0,1) (mkStdGen s)
    in (o == (1 :: Int))

-- Retorna un número aleatorio que determina la cantidad de piscinas de lava a generar
getNumberOfPools :: Int -> Int -> Int
getNumberOfPools n s =
    let (p, _) = randomR (3,6) (mkStdGen s)
    in p

-- Retorna un número aleatorio que determina la cantidad de murallas a generar
getNumberOfWalls :: Int -> Int -> Int
getNumberOfWalls n s =
    let (w, _) = randomR (5,2*n) (mkStdGen s)
    in w