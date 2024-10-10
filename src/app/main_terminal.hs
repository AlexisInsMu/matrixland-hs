{--Importar librerias--}
{-# LANGUAGE BlockArguments #-}

import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hFlush, stdout)
import System.Process (system)

-- main :: IO ()
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
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data ScreenClass = ScreenClass
  {text:: String
  , images :: String
  }
  deriving (Show, Generic)

data Info = Info
  { screen_prin :: ScreenClass
  , screen_1 :: ScreenClass
  , sreeen_menu :: ScreenClass
  } deriving (Show, Generic)

instance FromJSON ScreenClass
instance ToJSON ScreenClass

instance FromJSON Info
instance ToJSON Info
{-- DisplayScreen, imprime el contenido de un json interactuable con numeros--}
displayScreen :: FilePath -> (String -> IO())  -> IO ()
displayScreen filePath handleInput do
  clearScreen
  jsonData <- B.readFile filePath
  let _Data =  decode jsonData :: Maybe Info
  case _Data of 
    Just info -> do
      putStrLn $ text (screen_menu info)
      hFlush stdout
      op <- getLine
      handleInput op
    Nothing -> "Failed to aprse Json"


main :: IO ()
main = do
  displayScreen "../data/info.json" ejecutarOp

ejecutarOp :: String -> IO ()
ejecutarOp op = case op of
  "1" -> do
    liftIo clearScreen
    hFlush stdout -- Vaciar el búfer de salida
    op1 <- getLine
    ejecutarOp1 op1
  "2" -> do
    result <- system "clear"
    case result of
      ExitSuccess -> putStrLn ""
      ExitFailure _ -> putStrLn ""

    putStrLn ""
    putStrLn "---------MATRIZ IDENTIDAD---------"
    putStrLn "1) Aprender más"
    putStrLn "2) Quiz"
    putStrLn "3) Calculadora"
    putStrLn "4) Regresar al menú principal"
    putStr "¿Qué deseas aprender? "
    putStrLn ""
    hFlush stdout -- Vaciar el búfer de salida
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
    hFlush stdout -- Vaciar el búfer de salida
    op3 <- getLine
    ejecutarOp3 op3
  "4" -> do
    putStrLn "Saliendo..."
  _ -> do
    putStrLn "No válida. Intenta de nuevo."
    main

ejecutarOp1 :: String -> IO ()
ejecutarOp1 op1 = case op1 of
    "1" -> do
        putStrLn ""
        putStrLn "-------INFORMACIÓN-------"
        putStrLn "Antes que nada, debemos recordar que la potencia de una matriz no "
        siempre se puede calcular. Sólo es posible cuando 
        la matriz es cuadrada, es decir, cuando tiene el mismo número de filas que de columnas."
        putStrLn "La peculiaridad de la potenciación de las matrices es que, en muchas matrices, 
        las potencias siguen un patrón. Por ejemplo, las potencia n-ésima de una matriz diagonal 
        A es también una matriz diagonal cuyos elementos de la diagonal son las potencias 
        n-ésimas de los elementos de la diagonal de la matriz A:"
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
  _ -> do
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
  _ -> do
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
  _ -> do
    putStrLn ""
    putStrLn "No válida. Intenta de nuevo."
    putStrLn ""
    ejecutarOp "3"
