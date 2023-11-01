import System.Environment
import System.Random
import System.IO


-- funcion que crea una lista de listas, donde cada caracter de es un espacio en blanco
creaTablero :: Int -> [[Char]]
creaTablero n = replicate n (replicate n ' ')

randomCoordinate :: Int -> IO (Int, Int)
randomCoordinate n = do
    let m = n - 1
    x <- randomRIO (0, m)
    y <- randomRIO (0, m)
    return (x, y)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
        then putStrLn "Uso: ./max <n> <s>"
    else do
        let n = read (args !! 0) :: Int
            s = read (args !! 1) :: Int
        putStrLn ("n = " ++ show n ++ ", s = " ++ show s)
        let tablero = creaTablero n
        movement
        --let coordinate = randomCoordinate n
        --    coordinateString = show <$> coordinate
        --result <- coordinateString
        --putStrLn $ "Random coordinate: " ++ show result
        --showTup coordinate
        -- let player = setPlayer s

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
    | n == "W" = "Movido hacia arriba."
    | n == "A" = "Movido hacia la izquierda."
    | n == "S" = "Movido hacia abajo."
    | n == "D" = "Movido hacia la derecha."
    | n == "R" = "El mapa ha sido regenerado."
    | otherwise = "Use W, A, S, D o R."

-- setPlayer :: Int -> [Int]
-- setPlayer s = do
    -- Generar x entre 0 y n-1
    -- Generar y entre 0 y n-1
    
randomCoordinateWithSeedAndLimit :: Int -> Int -> ((Int, Int), StdGen)
randomCoordinateWithSeedAndLimit n s = let
    gen = mkStdGen s
    (x, gen') = randomRR (0, n) gen
    (y, gen'') = randomRR (0, n) gen'
    in ((x, y), gen'')