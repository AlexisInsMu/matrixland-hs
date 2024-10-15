{--Importar librerias--}
{-# LANGUAGE BlockArguments #-}

import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitSuccess)
import System.IO (hFlush, stdout)

import System.Console.Haskeline
    ( defaultSettings, getInputChar, outputStrLn, runInputT, InputT)
import System.Console.ANSI ( clearScreen, setCursorPosition )
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
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
  if ((dato1 == "Y" or dato1 == "y") and (dato2 == "Y" or dato2 == "y")) then 
    putStrLn "Lo hiciste bien !"
    threadDelay 3000000  -- Espera de 2 segundos
  else 
    



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
    putStrLn ""
    putStrLn "-------CALCULADORA PARA MULTIPLICACIÓN DE MATRICES-------"
    putStrLn ""
    displayScreen "./src/data/info.json" ejecutarOp screen_3 "" ""
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





