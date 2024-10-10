module Operations.Operaciones_matrix
    ( prodEscalar
    , multiMatriz
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