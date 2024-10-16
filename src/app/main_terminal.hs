{--Importar librerias--}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Control.Monad (replicateM)
import Data.Array (array)
import System.Console.ANSI ( clearScreen )
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Operations.Operaciones_matrix (multiMatriz, identMatriz, listToArray)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.List.Split (splitOn)
--import System.Process (system)

-- mainterminal :: IO ()
-- main = runInputT defaultSettings loop
--     where
--     loop :: InputT IO ()
--     loop = do
--       minput <- getInputChar "Press a key (q to quit): "
--       case minput of
--         Just 'q' -> outputStrLn "Exiting..."
--         Just c   -> do
--           liftIO clearScreen
--           liftIO $ setCursorPosition 0 0
--           outputStrLn ("Key pressed: " ++ [c])
--           loop
--         Nothing  -> loop


{-- Data class--}
data ScreenClass = ScreenClass
  {text:: String
  , images :: String
  }
  deriving (Show, Generic)


-- data ScreenQuiz = ScreenQuiz  
--   {text:: String
--   ,text2 :: String
--   , respuestas :: String
--   }
--   deriving (Show, Generic)

{--JSON data--}
data Info = Info
  { screen_select :: ScreenClass
  , screen_1 :: ScreenClass
  , screen_menu :: ScreenClass
  , screen_2 :: ScreenClass
  , screen_3 :: ScreenClass
  , screen_quiz1  :: ScreenClass
  , screen_quiz2  :: ScreenClass
  , screen_quiz3  :: ScreenClass
  , screen_option :: ScreenClass
  } deriving (Show, Generic)

instance FromJSON ScreenClass
instance ToJSON ScreenClass

instance FromJSON Info
instance ToJSON Info

-- Lambda function to extract dato1 and dato2 from a
extractDatos :: String -> (String, String)
extractDatos = (\a -> let [dato1, dato2] = splitOn "/" a in (dato1, dato2))

{-- DisplayScreen, imprime el contenido de un json interactuable con numeros--}
displayScreen :: FilePath -> (String  -> IO())  -> (Info -> ScreenClass) -> String -> String -> IO  ()
displayScreen filePath handleInput selectScreen control place_select = do
  clearScreen
  jsonData <- B.readFile filePath
  let _Data =  decode jsonData :: Maybe Info
  if control == "" then
    case _Data of
      Just info -> do
        let screen = selectScreen info
        putStrLn $ text screen
        hFlush stdout
        op <- getLine
        handleInput op
      Nothing -> do
        putStrLn "Failed to parse Json\n press q to exit"
        op <- getLine
        exitSuccess
  else if control == "+" then
    case _Data of
      Just info -> do
        let screen = selectScreen info
        putStrLn $ place_select
        putStrLn $ text screen
        putStrLn "\nPoner primera opcion: (Y/y)Yes , (N/n)No"
        hFlush stdout
        input1 <- getLine
        putStrLn "\nPoner segunda opcion: "
        hFlush stdout
        input0 <- getLine
        let dato1 = read input1:: String
        let dato2 = read input0:: String
        handleInput (dato1 ++ "/" ++ dato2)
        return ()

      Nothing -> do
        putStrLn "Failed to parse Json\n press q to exit"
        op <- getLine
        exitSuccess
  else if control == "-" then
    case _Data of
      Just info -> do
        let screen = selectScreen info
        putStrLn $ text screen
        hFlush stdout
        op <- getLine
        handleInput op

      Nothing -> do
        putStrLn "Failed to parse Json\n press q to exit"
        op <- getLine
        exitSuccess

  else
    case _Data of
      Just info -> do
        let screen = selectScreen info
        putStrLn $ text screen
        hFlush stdout
        op <- getLine
        handleInput control
      Nothing -> do
        putStrLn "Failed to parse Json\n press q to exit"
        op <- getLine
        exitSuccess

--- Ejecuta el menú de arranque 
ejecutarOp_a :: String -> IO ()
ejecutarOp_a op = case op of
  "1" -> do
    mainterminal
  "2" -> do
    --- aqui va main terminal
    putStrLn "Aqui va algo"
    liftIO exitSuccess
  "3" -> do
    clearScreen
    putStrLn "Saliendo..."
    liftIO exitSuccess
  _ -> do
    clearScreen
    putStrLn "No válida. Intenta de nuevo."
    main

ejecutarOp_q :: String -> IO()
ejecutarOp_q op = do
  let (dato1, dato2) = extractDatos op
  if (dato1 == "Y" || dato1 == "y") && (dato2 == "Y" || dato2 == "y") 
    then do
      putStrLn "Lo hiciste bien !"
      threadDelay 3000000  -- Espera de 2 segundos
      op <- getLine
      return ()
    else do
      putStrLn "aqui"
      op <- getLine
      return ()




ejecutarOp :: String -> IO ()
ejecutarOp op = case op of
  "1" -> do
    displayScreen "./src/data/info.json" ejecutarOp1 screen_option "+" "Potencia de matrices"
  "2" -> do
    displayScreen "./src/data/info.json" ejecutarOp2 screen_option "+" "Matriz identidad"
  "3" -> do
    displayScreen "./src/data/info.json" ejecutarOp3 screen_option "+" "Multiplicación de matrices"
  "4" -> do
    clearScreen
    putStrLn "Saliendo..."
    liftIO exitSuccess
  _ -> do
    clearScreen
    putStrLn "No válida. Intenta de nuevo."
    mainterminal

ejecutarOp1 :: String -> IO ()
ejecutarOp1 op1 = case op1 of
  "1" -> do
    displayScreen "./src/data/info.json" ejecutarOp screen_1 "1" ""
  "2" -> do
    displayScreen "./src/data/info.json" ejecutarOp screen_quiz1 "-" ""
  "3" -> do
    putStrLn ""
    putStrLn "-------CALCULADORA PARA POTENCIAS DE MATRICES-------"
    putStrLn ""
    displayScreen "./src/data/info.json" ejecutarOp screen_1 "" ""
  "4" -> do
    putStrLn "Regresando"
    threadDelay 2000000  -- Espera de 2 segundos
    mainterminal
  _ -> do
    putStrLn ""
    putStrLn "No válida. Intenta de nuevo."
    putStrLn ""
    threadDelay 2000000  -- Espera de 2 segundos
    displayScreen "./src/data/info.json" ejecutarOp1 screen_option "+" "Potencia de matrices"

ejecutarOp2 :: String -> IO ()
ejecutarOp2 op2 = case op2 of
  "1" -> do
    displayScreen "./src/data/info.json" ejecutarOp screen_2 "2" ""
  "2" -> do
    displayScreen "./src/data/info.json" ejecutarOp screen_quiz2 "" ""
  "3" -> do
    putStrLn ""
    putStrLn "-------CALCULADORA PARA MATRICES IDENTIDAD-------"
    putStrLn ""
    displayScreen "./src/data/info.json" ejecutarOp screen_2 "" ""
  "4" -> do
    putStrLn "Regrasando"
    threadDelay 2000000  -- Espera de 2 segundos
    mainterminal
  _ -> do
    putStrLn ""
    putStrLn "No válida. Intenta de nuevo."
    putStrLn ""
    threadDelay 2000000  -- Espera de 2 segundos
    displayScreen "./src/data/info.json" ejecutarOp2 screen_option "+" "Matriz identidad"

ejecutarOp3 :: String -> IO ()
ejecutarOp3 op3 = case op3 of
  "1" -> do
    displayScreen "./src/data/info.json" ejecutarOp screen_3 "3" ""
  "2" -> do
    putStrLn ""
    putStrLn "-------QUIZ MULTIPLICACIÓN DE MATRICES-------"
    putStrLn ""
    displayScreen "./src/data/info.json" ejecutarOp screen_quiz3 "" ""
  "3" -> do
    mainmulti
  "4" -> do
    putStrLn "Regrasando"
    threadDelay 2000000  -- Espera de 2 segundos
    mainterminal
  _ -> do
    putStrLn ""
    putStrLn "No válida. Intenta de nuevo."
    putStrLn ""
    threadDelay 2000000  -- Espera de 2 segundos
    displayScreen "./src/data/info.json" ejecutarOp3 screen_option "+" "Multiplicación de matrices"

--main terminal version
mainterminal :: IO ()
mainterminal = do
  displayScreen "./src/data/info.json" ejecutarOp screen_menu "" ""


-- Main principal
main :: IO ()
main = do
  displayScreen "./src/data/info.json" ejecutarOp_a screen_select "" ""


--Main de la calculadora de multiplicación de matrices
mainmulti :: IO ()
mainmulti = do
    putStrLn ""
    putStrLn "-------CALCULADORA PARA MULTIPLICACIÓN DE MATRICES-------"
    putStrLn ""
    -- Solicitar al usuario las dimensiones de la primera matriz
    putStrLn "Recuerde que para la multiplicación las matrices deben ser cuadradas."
    putStrLn "Ingrese las dimensiones de la primera matriz (filas columnas):"
    [m, n] <- fmap (map read . words) getLine :: IO [Int]

    -- Solicitar los elementos de la primera matriz
    putStrLn "Ingrese los elementos de la primera matriz (por filas) (Ingrese una fila ENTER ingrese la otra):"
    elementosP <- replicateM m (fmap (map read . words) getLine :: IO [Int])
    let p = array ((1, 1), (m, n)) [((i, j), elementosP !! (i - 1) !! (j - 1)) | i <- [1..m], j <- [1..n]]

    -- Solicitar al usuario las dimensiones de la segunda matriz
    putStrLn "Ingrese las dimensiones de la segunda matriz (filas columnas):"
    [k, l] <- fmap (map read . words) getLine :: IO [Int]

    -- Verificar si las matrices pueden ser multiplicadas
    if n /= k
        then  do

          putStrLn "Las matrices no se pueden multiplicar. El número de columnas de la primera debe ser igual al número de filas de la segunda."
          threadDelay 3000000
          putStrLn "\n\n Reintentando"
          mainmulti
        else do
            -- Solicitar los elementos de la segunda matriz
            putStrLn "Ingrese los elementos de la segunda matriz (por filas):"
            elementosQ <- replicateM k (fmap (map read . words) getLine :: IO [Int])
            let q = array ((1, 1), (k, l)) [((i, j), elementosQ !! (i - 1) !! (j - 1)) | i <- [1..k], j <- [1..l]]

            -- Multiplicar las matrices
            let resultado = multiMatriz p q
            putStrLn "El resultado de la multiplicación es:"
            print resultado
            displayScreen "./src/data/info.json" ejecutarOp3 screen_option "+" "Multiplicación de matrices"



--Main para la calculadora de matriz identidad.
mainidentity :: IO ()
mainidentity = do
  putStrLn "Recuerde que las matrices identidad son cuadradas."
  n <- readLn :: IO Int
  putStrLn "Ingrese el tamaño de su matriz identidad."
  --Generamos la matriz identidad al llamar la función.
  let matrizI = identMatriz n
  --Convertimos la lista en una listas a un array.
  let matrizIA = listToArray matrizI
  --Mandamos a pantalla el resultado.
  putStrLn "La matriz identidad es:"
  print matrizIA

--Main para la calculadora de exponenciacion binaria.
mainexpo :: do()
mainexpo = do
  -- Solicitar al usuario las dimensiones de la matriz
  putStrLn "Recuerde que la matriz debe ser cuadrada."
  putStrLn "Ingrese las dimensiones de la primera matriz (filas columnas):"
  x <- readLn :: IO Int

  -- Solicitar los elementos de la matriz
  putStrLn "Ingrese los elementos de la matriz (por filas) (Ingrese una fila ENTER ingrese la otra):"
  elementosP <- replicateM x (fmap (map read . words) getLine :: IO [Int])
  let p = array ((1, 1), (x, x)) [((i, j), elementosP !! (i - 1) !! (j - 1)) | i <- [1..x], j <- [1..x]]

  -- Solicitar los elementos de la matriz
  putStrLn "Ingrese el exponente:"
  n <- readLn :: IO Int

  let result = exponention p n
  putStrLn "El resultado de la exponenciacion es:"
            print resultado
