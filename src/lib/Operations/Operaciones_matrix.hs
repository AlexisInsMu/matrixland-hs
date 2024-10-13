module Operations.Operaciones_matrix
    ( prodEscalar
    , multiMatriz
    , identMatriz
    ) where

import Data.Array

-- Definimos una función para obtener el producto escalar de los vectores.
prodEscalar :: Num a => Array Int a -> Array Int a -> a
prodEscalar x y =
    sum [i * j | (i, j) <- zip (elems x) (elems y)]

-- Multiplicación de matrices
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

identMatriz :: Int -> [[Int]]
identMatriz n = map(\i -> map (\j -> if i == j then 1 else 0) [0..n-1]) [0..n-1]

-- Funciones auxiliares para obtener filas y columnas
fila :: Int -> Array (Int, Int) a -> Array Int a
fila i p = ixmap (1, n) (\j -> (i, j)) p
    where (_, (m, n)) = bounds p

columna :: Int -> Array (Int, Int) a -> Array Int a
columna j q = ixmap (1, m) (\i -> (i, j)) q
    where (_, (m, n)) = bounds q

noFilas :: Array (Int, Int) a -> Int
noFilas p = m
    where (_, (m, _)) = bounds p

noColumnas :: Array (Int, Int) a -> Int
noColumnas q = n
    where (_, (_, n)) = bounds q



