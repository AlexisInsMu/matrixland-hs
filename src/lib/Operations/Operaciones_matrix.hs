module Operations.Operaciones_matrix
    ( prodEscalar
    , multiMatriz
    , identMatriz
    , exponention
    , listToArray
    ) where

import Data.Array

-- Función para calcular el producto escalar de dos vectores
prodEscalar :: Num a => Array Int a -> Array Int a -> a 
prodEscalar x y =
    sum [i * j | (i, j) <- zip (elems x) (elems y)]

-- Función para multiplicar matrices
multiMatriz :: Num a => Array (Int, Int) a -> Array (Int, Int) a -> Array (Int, Int) a
multiMatriz p q =
    array ((1, 1), (m, n))
          [((i, j), prodEscalar (fila i p) (columna j q)) | i <- [1..m], j <- [1..n]]
    where
        m = noFilas p
        n = noColumnas q

-- Matriz Identidad:
-- n guarda el tamaño de la matriz, ya que para la identidad es nxn
-- El primer <map> itera sobre cada fila i. Para c/valor, genera un fila con una lambda.
-- El segundo <map>, para cada fila respectivamente itera sobre cada columna j
-- y la lambda define si en es posición va un 1 o 0. Esto lo hace con el if.
-- Si los indice i y j son iguales (osea estan en la diagonal principal)
-- pone un cero, si son diferentes pone un cero.
-- Y así de vuelve una lista de n listas con n valores.

--map: Le aplica la función de los parentesis a la lista que ponemos, elemento a elemento.

identMatriz :: Num a => Int -> [[a]]
identMatriz n = map (\i -> map (\j -> if i == j then 1 else 0) [0..n-1]) [0..n-1]

-- Funciones auxiliares para obtener filas y columnas
-- Función para obtener una fila de la matriz
fila :: Int -> Array (Int, Int) a -> Array Int a
fila i p = ixmap (1, n) (\j -> (i, j)) p
    where (_, (m, n)) = bounds p

-- Función para obtener una columna de la matriz
columna :: Int -> Array (Int, Int) a -> Array Int a
columna j q = ixmap (1, m) (\i -> (i, j)) q
    where (_, (m, n)) = bounds q

-- Función para obtener el número de filas
noFilas :: Array (Int, Int) a -> Int
noFilas p = m
    where (_, (m, _)) = bounds p

-- Función para obtener el número de columnas
noColumnas :: Array (Int, Int) a -> Int
noColumnas q = n
    where (_, (_, n)) = bounds q

arrayToList :: Array (Int, Int) Int -> [[Int]]
arrayToList arr = [[arr ! (i, j) | j <- [0..n-1]] | i <- [0..m-1]]
  where
    m = noFilas arr
    n = noColumnas arr

listToArray :: Num a => [[a]] -> Array (Int, Int) a
listToArray xss = array ((0, 0), (rows - 1, cols - 1)) [((i, j), xss !! i !! j) | i <- [0..rows-1], j <- [0..cols-1]]
  where
    rows = length xss
    cols = length (head xss)


{-- Potenciación de matrices --}
{--

--}

{-- Fast EXPO
    Esta función se encarga de realizar la potenciación de matrices de forma rápida.
    Recibe una matriz y un número entero, y devuelve la matriz elevada a la potencia n.
    La función se basa en la propiedad de que si n es par, entonces A^n = (A^2)^(n/2), y si n es impar, entonces A^n = A * A^(n-1).
    De esta forma, se reduce el número de multiplicaciones necesarias para obtener la matriz elevada a la potencia n.
 --}
fastExpo :: Num a => Array (Int, Int) a -> Int -> Array (Int, Int) a
fastExpo p 1 = p
fastExpo p n
    | even n = fastExpo (multiMatriz p p) (n `div` 2)
    | otherwise = multiMatriz p (fastExpo p (n - 1))

-- Función para obetner el inverso de una matriz
inverso :: Num a => Array (Int, Int) a -> Array (Int, Int) a
inverso p = array ((1, 1), (m, n)) [((i, j),- (p ! (i, j))) | i <- [1..m], j <- [1..n]]
    where
        m = noFilas p
        n = noColumnas p

exponention :: Num a => Array (Int, Int) a -> Int -> Array (Int, Int) a
exponention p n
    | n == 0 = listToArray (identMatriz (noFilas p))
    | n < 0 = fastExpo (inverso p) (-n)
    | otherwise = fastExpo p n