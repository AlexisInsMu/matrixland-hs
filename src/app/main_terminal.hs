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
displayScreen :: FilePath -> (String -> IO())  -> (Info -> ScreenClass) -> String -> IO  ()
displayScreen filePath handleInput selectScreen control do
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
      Nothing -> "Failed to parse Json"
  else
    case _Data of 
      Just info -> do
        let screen = selectScreen info
        putStrLn $ text screen
        hFlush stdout
        handleInput control
        waitForAnyKey
      Nothing -> "Failed to parse Json"


main :: IO ()
main = do
  displayScreen "../data/info.json" ejecutarOp sreeen_menu ""

ejecutarOp :: String -> IO ()
ejecutarOp op = case op of
  "1" -> do
    displayScreen "../data/info.json" ejecutarOp1 screen_1 ""
  "2" -> do
    displayScreen "../data/info.json" ejecutarOp2 screen_2 ""
  "3" -> do 
    displayScreen "../data/info.json" ejecutarOp3 screen_3 ""
  "4" -> do
    clearScreen
    putStrLn "Saliendo..."
  _ -> do
    clearScreen
    putStrLn "No válida. Intenta de nuevo."
    main

ejecutarOp1 :: String -> IO ()
ejecutarOp1 op1 = case op1 of
    "1" -> do
        displayScreen "../data/info.json" ejecutarOp screen_info_1 "1"
        putStrLn ""
        putStrLn "-------INFORMACIÓN-------"
        putStrLn "Antes que nada, debemos recordar que la potencia de una matriz no "
        "siempre se puede calcular. Sólo es posible cuando "
        "la matriz es cuadrada, es decir, cuando tiene el mismo número de filas que de columnas."
        putStrLn "La peculiaridad de la potenciación de las matrices es que, en muchas matrices, 
        las potencias siguen un patrón. Por ejemplo, las potencia n-ésima de una matriz diagonal 
        A es también una matriz diagonal cuyos elementos de la diagonal son las potencias ""
        n-ésimas de los elementos de la diagonal de la matriz A:"
        ejecutarOp "1"
    "2" -> do
        displayScreen "../data/info.json" ejecutarOp screen_info_2 "1"
        putStrLn ""
        putStrLn "-------QUIZ POTENCIA DE MATRICES-------"
        putStrLn ""
        ejecutarOp "1"
    "3" -> do
        displayScreen "../data/info.json" ejecutarOp screen_info_3 "1"
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
