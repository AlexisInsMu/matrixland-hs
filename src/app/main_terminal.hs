{--Importar librerias--}
--Omite los aprentesis en funciones como lambdas o do.
{-# LANGUAGE BlockArguments #-}
-- Ayuda a ignorar las advertencias innecesarias.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
--Ignora las recomendaciones con el camelCase (mayus en c/palabra de un identificador).
{-# HLINT ignore "Use camelCase" #-}

--Para terminar el programa.
import System.Exit (exitSuccess)
--Para vacias el buffer de salida, y para referirse a la salida estándar.
import System.IO (hFlush, stdout)
--replicateM: ejecuta una función unidad que mapea un valor de un 
--tipo subyacente a un valor, repetidas veces, devolviendo una lista.
import Control.Monad (replicateM)
--Permite crear arreglos.
import Data.Array (array)
--Para limpiar la consola.
import System.Console.ANSI ( clearScreen )
--Facilita la ejecución de op. in/out dentro de un contexto monádico.
import Control.Monad.IO.Class (liftIO)
--Termina la ejecución por un número de microsegundos.
import Control.Concurrent (threadDelay)
--Importa las funciones del archivo Operaciones_matrix.hs
import Operations.Operaciones_matrix (multiMatriz, identMatriz, listToArray, exponentation, imprimir_bonito)
--Ayuda a codificar y decodificar JSON.
import Data.Aeson ( decode, FromJSON, ToJSON )
--Da soporte para la derivación de instancis de clases de tipo.
import GHC.Generics ( Generic )
--Ayuda a dar soporta para el manejo de cadanas de bytes grandes.
import qualified Data.ByteString.Lazy as B
--Sirve para dividir una cadena en una lista de subcadenas utilizando un delimitador.
import Data.List.Split (splitOn)
-- Data.List: contiene funciones para trabajar con listas.
import Data.List (elemIndex)
-- Data.Maybe: contiene funciones para trabajar con valores que pueden ser nulos.
import Data.Maybe (fromMaybe)
-- Data.Char: contiene funciones para manipular caracteres.
import Data.Char (toLower)

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
--Creamos un objeto tipo ScreenClass que va a contener dos campos.
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
--Son todas los objetos que vamos a llamar del archivo JSON.
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

--Estas instancias va a permitir que Haskell pueda convertir los datos
--del JSON en ScreenClass y Info para que podamos trabajarlos.
instance FromJSON ScreenClass
instance ToJSON ScreenClass

instance FromJSON Info
instance ToJSON Info

-- Lambda function to extract dato1 and dato2 from a
--extractDatos :: String -> (String, String, String)
--extractDatos = \a -> let [where_select ,dato1, dato2] = splitOn "/" a in (where_select,dato1, dato2)

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
        putStr "\t"
        putStrLn $ place_select
        putStrLn $ text screen
        putStrLn "\nPoner primera opcion: (Y/y)Yes , (N/n)No"
        hFlush stdout
        input1 <- getLine
        putStrLn "\nPoner segunda opcion: "
        hFlush stdout
        input0 <- getLine
        let dato1 = input1
        let dato2 = input0
        handleInput (place_select ++ "/"++ dato1 ++ "/" ++ dato2)

      Nothing -> do
        putStrLn "Failed to parse Json\n press q to exit"
        op <- getLine
        exitSuccess
  else if control == "-" then
    case _Data of
      Just info -> do
        putStrLn $ place_select
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


-- Función para buscar una cadena en una lista y retornar el índice
buscarIndice :: String -> [String] -> Int
buscarIndice str lista = fromMaybe (-1) (elemIndex str lista)

toLowerString :: String -> String
toLowerString  = map toLower 

--Ejecuta este manu para los quizes
ejecutarOp_q :: String -> IO()
ejecutarOp_q op = do
  let (where_select,dato1, dato2) = (\a -> let [where_select ,dato1, dato2] = splitOn "/" a in (where_select,dato1, dato2)) op
  let dato1_lower = toLowerString dato1
  let dato2_lower = toLowerString dato2
  let lista = ["Potencia de matrices", "Matriz identidad","Multiplicacion de matrices"]
  let lista_screen = [["Y", "N"], ["N", "Y"], ["Y", "N"]]
  let index = buscarIndice where_select lista

  putStrLn "Respuestas correctas:"
  putStrLn $ "1. " ++ dato1
  putStrLn $ "2. " ++ dato2
  putStrLn $ "Respuestas correctas:"
  putStrLn $ "1. " ++ lista_screen !! index !! 0
  putStrLn $ "2. " ++ lista_screen !! index !! 1
  hFlush stdout
  if (dato1_lower == lista_screen !! index !! 0) && (dato2_lower == lista_screen !! index !! 1)
    then do
      putStrLn "Lo hiciste bien !"
      threadDelay 3000000  -- Espera de 2 segundos
      putStrLn "\n\n presiona algo para regresar"
      hFlush stdout
      if where_select == "Potencia de matrices"
        then do
          op <- getLine
          displayScreen "./src/data/info.json" ejecutarOp1 screen_option "-" "Potencia de matrices"
      else if where_select == "Matriz identidad"
        then do
          op <- getLine
          displayScreen "./src/data/info.json" ejecutarOp2 screen_option "-" "Matriz identidad"
      else if where_select == "Multiplicacion de matrices"
        then do
          op <- getLine
          displayScreen "./src/data/info.json" ejecutarOp1 screen_option "-" "Multiplicacion de matrices"
      else 
        do 
        putStrLn "Regresando"
        hFlush stdout
        op <- getLine
        threadDelay 3000000  -- Espera de 2 segundos
        mainterminal
  else do
      putStrLn "Lo hiciste mal !"
      putStrLn "Vuelvelo a intentar"
      threadDelay 3000000  -- Espera de 2 segundos
      if where_select == "Potencia de matrices"
        then do
          displayScreen "./src/data/info.json" ejecutarOp_q screen_quiz1 "+" "Potencia de matrices"
      else if where_select == "Matriz identidad"
        then do
          displayScreen "./src/data/info.json" ejecutarOp_q screen_quiz2 "+" "Matriz identidad"
      else if where_select == "Multiplicacion de matrices"
        then do
          displayScreen "./src/data/info.json" ejecutarOp_q screen_quiz3 "+" "Multiplicacion de matrices"
      else 
        do 
        putStrLn "Regresando"
        hFlush stdout
        op <- getLine
        threadDelay 3000000  -- Espera de 2 segundos
        mainterminal



--Ejecuta el menu principal/de bienvenida.
ejecutarOp :: String -> IO ()
ejecutarOp op = case op of
  "1" -> do
    displayScreen "./src/data/info.json" ejecutarOp1 screen_option "-" "Potencia de matrices"
  "2" -> do
    displayScreen "./src/data/info.json" ejecutarOp2 screen_option "-" "Matriz identidad"
  "3" -> do
    displayScreen "./src/data/info.json" ejecutarOp3 screen_option "-" "Multiplicacion de matrices"
  "4" -> do
    clearScreen
    putStrLn "Saliendo..."
    liftIO exitSuccess
  _ -> do
    clearScreen
    putStrLn "No válida. Intenta de nuevo."
    mainterminal

--Ejecuta el submenu de la opción de Potencia de matrices
ejecutarOp1 :: String -> IO ()
ejecutarOp1 op1 = case op1 of
  "1" -> do
    displayScreen "./src/data/info.json" ejecutarOp screen_1 "1" ""
  "2" -> do
    displayScreen "./src/data/info.json" ejecutarOp_q screen_quiz1 "+" "Potencia de matrices"
  "3" -> do
    mainexpo
  "4" -> do
    putStrLn "Regresando"
    threadDelay 3000000  -- Espera de 2 segundos
    mainterminal
  _ -> do
    putStrLn ""
    putStrLn "No válida. Intenta de nuevo."
    putStrLn ""
    threadDelay 3000000  -- Espera de 2 segundos
    displayScreen "./src/data/info.json" ejecutarOp1 screen_option "-" "Potencia de matrices"

--Ejecuta el submenu de la opción de Matriz identidad. 
ejecutarOp2 :: String -> IO ()
ejecutarOp2 op2 = case op2 of
  "1" -> do
    displayScreen "./src/data/info.json" ejecutarOp screen_2 "2" ""
  "2" -> do
    displayScreen "./src/data/info.json" ejecutarOp screen_quiz2 "+" "Matriz identidad"
  "3" -> do
    mainidentity
  "4" -> do
    putStrLn "Regrasando"
    threadDelay 2000000  -- Espera de 2 segundos
    mainterminal
  _ -> do
    putStrLn ""
    putStrLn "No válida. Intenta de nuevo."
    putStrLn ""
    threadDelay 2000000  -- Espera de 2 segundos
    displayScreen "./src/data/info.json" ejecutarOp2 screen_option "-" "Matriz identidad"

--Ejecuta el submenu de la opción de Multiplicacion de matrices
ejecutarOp3 :: String -> IO ()
ejecutarOp3 op3 = case op3 of
  "1" -> do
    displayScreen "./src/data/info.json" ejecutarOp screen_3 "3" ""
  "2" -> do
    displayScreen "./src/data/info.json" ejecutarOp_q screen_quiz3 "+" "Multiplicacion de matrices"
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
    displayScreen "./src/data/info.json" ejecutarOp3 screen_option "-" "Multiplicacion de matrices"

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
    --words: ayuda para dividir en palabras, read: convierte los datos leidos en enteros
    --Se guardan en la lista [m,n]
    putStrLn "Recuerde que para la multiplicación las matrices deben ser cuadradas."
    putStrLn "Ingrese las dimensiones de la primera matriz (filas columnas):"
    [m, n] <- fmap (map read . words) getLine :: IO [Int]

    -- Solicitar los elementos de la primera matriz
    --Utilizamoas replicateM para pedir una lista de listas, c/fila es un lista de enteros.
    --p va a guardad la matriz que se a ir armando con los elementosP de acurdo con los indices
    --de i y j.
    putStrLn "Ingrese los elementos de la primera matriz (por filas) (Ingrese una fila ENTER ingrese la otra):"
    elementosP <- replicateM m (fmap (map read . words) getLine :: IO [Int])
    let p = array ((1, 1), (m, n)) [((i, j), elementosP !! (i - 1) !! (j - 1)) | i <- [1..m], j <- [1..n]]

    -- Solicitar al usuario las dimensiones de la segunda matriz
    putStrLn "Ingrese las dimensiones de la segunda matriz (filas columnas):"
    [k, l] <- fmap (map read . words) getLine :: IO [Int]

    -- Verificar si las matrices pueden ser multiplicadas
    --Como deben ser matriz cuadradas sus dimensiones tienen que ser las mismas.
    --Si son diferentes lanza un mensaje y vuelve a llamar al mainmulti para que se vuelva a intentar.
    if n /= k
        then  do
          putStrLn "Las matrices no se pueden multiplicar. El número de columnas de la primera debe ser igual al número de filas de la segunda."
          threadDelay 3000000
          putStrLn "\n\n Reintentando"
          mainmulti
        else do
          -- Solicitar los elementos de la segunda matriz
            --Se aplica los mismo que en la primera matriz
          putStrLn "Ingrese los elementos de la segunda matriz (por filas):"
          elementosQ <- replicateM k (fmap (map read . words) getLine :: IO [Int])
          let q = array ((1, 1), (k, l)) [((i, j), elementosQ !! (i - 1) !! (j - 1)) | i <- [1..k], j <- [1..l]]
          -- Multiplicar las matrices
            --En resultado vamos a guardar los que nos envie la función multiMatriz al haberle enviado las dos matrices
            --como parametros. Se imprime el resultado. Se presiona 4 para regresar al submenu.
          let resultado = multiMatriz p q
          putStrLn "El resultado de la multiplicación es:"
          imprimir_bonito resultado
          putStrLn ""
          putStrLn "Presiona algo para regresar al menú."
          op <- getLine
          displayScreen "./src/data/info.json" ejecutarOp3 screen_option "-" "Multiplicacion de matrices"

--Main para la calculadora de matriz identidad.
mainidentity :: IO ()
mainidentity = do
  putStrLn "Recuerde que las matrices identidad son cuadradas."
  putStrLn "Ingrese el tamaño de su matriz identidad."
  hFlush stdout
  n <- readLn :: IO Int
  --Generamos la matriz identidad al llamar la función.
  let matrizI = identMatriz n
  --Convertimos la lista en una listas a un array.
  let matrizIA = listToArray matrizI
  --Mandamos a pantalla el resultado.
  putStrLn "La matriz identidad es:"
  imprimir_bonito matrizIA
  putStrLn ""
  putStrLn "Presiona algo para regresar al menú."
  hFlush stdout
  op <- getLine
  displayScreen "./src/data/info.json" ejecutarOp2 screen_option "-" "Matriz identidad"

--Main para la calculadora de exponenciacion binaria.
mainexpo :: IO ()
mainexpo = do
  -- Solicitar al usuario las dimensiones de la matriz
  putStrLn "Recuerde que la matriz debe ser cuadrada."
  putStrLn "Ingrese las dimensiones de la primera matriz (filas columnas):"
  x <- readLn :: IO Int

  -- Solicitar los elementos de la matriz
  putStrLn "Ingrese los elementos de la matriz (por filas) (Ingrese una fila ENTER ingrese la otra):"
  elementosP <- replicateM x (fmap (map read . words) getLine :: IO [Int])
  let p = array ((1, 1), (x, x)) [((i, j), elementosP !! (i - 1) !! (j - 1)) | i <- [1..x], j <- [1..x]]

  -- Solicitar el exponente.
  putStrLn "Ingrese el exponente:"
  n <- readLn :: IO Int

  --En resultado vamos a guardar los que nos envie la función exponention al haberle enviado la matriz y
  --el exponente como parametros. Se imprime el resultado. Se presiona 4 para regresar al submenu.
  let result = exponentation p n
  putStrLn "El resultado de la exponenciacion es:"
  imprimir_bonito result
  putStrLn ""
  putStrLn "Presiona algo para regresar al menú."
  op <- getLine
  displayScreen "./src/data/info.json" ejecutarOp1 screen_option "-" "Potencia de matrices"
