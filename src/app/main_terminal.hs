{--Importar librerias--}
import Data.Array
import System.IO (hFlush, stdout)
<<<<<<< HEAD

import System.Console.Haskeline
    ( defaultSettings, getInputChar, outputStrLn, runInputT, InputT )
import System.Console.ANSI ( clearScreen, setCursorPosition )
import Control.Monad.IO.Class (liftIO)



main :: IO ()
main = runInputT defaultSettings loop
    where 
    loop :: InputT IO ()
    loop = do
      minput <- getInputChar "Press a key (q to quit): "
      case minput of
        Just 'q' -> outputStrLn "Exiting..."
        Just c   -> do
          liftIO clearScreen
          liftIO $ setCursorPosition 0 0
          outputStrLn ("Key pressed: " ++ [c])
          loop
        Nothing  -> loop
        
=======

main :: IO ()
>>>>>>> f75355a6cbf3500a630c39371f40ef2e9932e7c7
main = do
          putStrLn ""
          putStrLn "---------BIENVENIDO---------"
          putStrLn "---------Menú principal---------"
          putStrLn "1) Potencia de matrices"
          putStrLn "2) Matriz identidad"
          putStrLn "3) Multiplicación de matrices"
          putStrLn "4) Salir"
          putStr "¿Qué deseas aprender? "
          putStrLn ""
          hFlush stdout  -- Vaciar el búfer de salida
          op <- getLine
          ejecutarOp op

ejecutarOp :: String -> IO ()
ejecutarOp op = case op of
    "1" -> do
            result <- system "clear"
            case result of
                ExitSuccess   -> putStrLn ""
                ExitFailure _ -> putStrLn ""

            putStrLn ""
            putStrLn "---------POTENCIA DE MATRICES---------"
            putStrLn "1) Aprender más"
            putStrLn "2) Quiz"
            putStrLn "3) Calculadora"
            putStrLn "4) Regresar al menú principal"
            putStr "¿Qué deseas aprender? "
            putStrLn ""
            hFlush stdout  -- Vaciar el búfer de salida
            op1 <- getLine
            ejecutarOp1 op1

    "2" -> do
            result <- system "clear"
            case result of
                ExitSuccess   -> putStrLn ""
                ExitFailure _ -> putStrLn ""
            
            putStrLn ""
            putStrLn "---------MATRIZ IDENTIDAD---------"
            putStrLn "1) Aprender más"
            putStrLn "2) Quiz"
            putStrLn "3) Calculadora"
            putStrLn "4) Regresar al menú principal"
            putStr "¿Qué deseas aprender? "
            putStrLn ""
            hFlush stdout  -- Vaciar el búfer de salida
            op2 <- getLine
            ejecutarOp2 op2

    "3" -> do
            putStrLn ""
            putStrLn "---------MULTIPLICACIÓN DE MATRICES---------"
            putStrLn "1) Aprender más"
            putStrLn "2) Quiz"
            putStrLn "3) Calculadora"
            putStrLn "4) Regresar al menú principal"
            putStr "¿Qué deseas aprender? "
            putStrLn ""
            hFlush stdout  -- Vaciar el búfer de salida
            op3 <- getLine
            ejecutarOp3 op3

    "4" -> do
            putStrLn "Saliendo..."
    _   -> do
            putStrLn "No válida. Intenta de nuevo."
            main

ejecutarOp1 :: String -> IO ()
ejecutarOp1 op1 = case op1 of
    "1" -> do
        putStrLn ""
        putStrLn "-------INFORMACIÓN-------"
        putStrLn ""
        ejecutarOp "1"
    "2" -> do
        putStrLn ""
        putStrLn "-------QUIZ POTENCIA DE MATRICES-------"
        putStrLn ""
        ejecutarOp "1"
    "3" -> do
        putStrLn ""
        putStrLn "-------CALCULADORA PARA POTENCIAS DE MATRICES-------"
        putStrLn ""
        ejecutarOp "1"
    "4" -> do
        putStrLn ""
        main
    _   -> do
        putStrLn ""
        putStrLn "No válida. Intenta de nuevo."
        putStrLn ""
        ejecutarOp "1"

ejecutarOp2 :: String -> IO ()
ejecutarOp2 op2 = case op2 of
    "1" -> do
        putStrLn ""
        putStrLn "-------INFORMACIÓN-------"
        putStrLn ""
        ejecutarOp "2"
    "2" -> do
        putStrLn ""
        putStrLn "-------QUIZ MATRIZ IDENTIDAD-------"
        putStrLn ""
        ejecutarOp "2"
    "3" -> do
        putStrLn ""
        putStrLn "-------CALCULADORA PARA MATRICES IDENTIDAD-------"
        putStrLn ""
        ejecutarOp "2"
    "4" -> do
        putStrLn ""
        main
    _   -> do
        putStrLn ""
        putStrLn "No válida. Intenta de nuevo."
        putStrLn ""
        ejecutarOp "2"

ejecutarOp3 :: String -> IO ()
ejecutarOp3 op3 = case op3 of
    "1" -> do
        putStrLn ""
        putStrLn "-------INFORMACIÓN-------"
        putStrLn ""
        ejecutarOp "3"
    "2" -> do
        putStrLn ""
        putStrLn "-------QUIZ MULTIPLICACIÓN DE MATRICES-------"
        putStrLn ""
        ejecutarOp "3"
    "3" -> do
        putStrLn ""
        putStrLn "-------CALCULADORA PARA MULTIPLICACIÓN DE MATRICES-------"
        putStrLn ""
        ejecutarOp "3"
    "4" -> do
        putStrLn ""
        main
    _   -> do
        putStrLn ""
        putStrLn "No válida. Intenta de nuevo."
        putStrLn ""
        ejecutarOp "3"

{--Vectores son tablas cuyos indice son no. naturales--}
type Vector a = Array Int a

{--Matrices son tablas cuyos indice son tuplas de no. naturales--}
type Matriz a = Array (Int, Int) a

{--Definimos una función para determinar el número de filas 
de una matriz--}

noFilas :: Matriz a -> Int
noFilas = fst . snd . bounds

{--Definimos una función para determinar el número de columnas 
de una matriz--}

noColumnas :: Matriz a -> Int
noColumnas = snd . snd . bounds

{--Definimos una función para seleccionar una fila que queramos
mandar a llamar--}
fila :: Int -> Matriz a -> Vector a
fila i p = array (1,n) [(j,p!(i,j)) | j <- [1..n]]
    where n = noColumnas p

{--Definimos una función para seleccionar una columna que queramos
mandar a llamar--}
columna :: Int -> Matriz a -> Vector a
columna j p = array (1,m) [(i,p!(i,j)) | i <- [1..m]]
    where m = noFilas p

{--Definimo un función que nos va los elemento de un vector,
es decir, la lista con los elementos del vector--}
vectorList :: Vector a -> [a]
vectorList = elems

{--Definimos una función para obtener el producto escalar de los vectores.--}
prodEscalar :: Num a => Vector a -> Vector a -> a
prodEscalar x y =
    sum [i*j | (i,j) <- zip (elems x) (elems y)]

{--Multiplicación de matrices--}
multiMatriz :: Num a => Matriz a -> Matriz a -> Matriz a
multiMatriz p q =
    array ((1,1), (m,n))
          [((i,j), prodEscalar (fila i p) (columna j q)) | i <- [1..m], j <- [1..n]]
    where m = noFilas p
          n = noColumnas q

