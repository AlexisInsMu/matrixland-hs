module Operations.Operaciones_matrix
    ( prodEscalar
    , multiMatriz
    ) where

import Data.Array ( array, bounds, elems, ixmap, Array )

identity :: Num a => Int -> Array (Int, Int) a
identity n = array ((1, 1), (n, n)) [((i, j), fromIntegral (fromEnum (i == j))) | i <- [1..n], j <- [1..n]]

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
inverso p = array ((1, 1), (m, n)) [((i, j), -p!(i, j)) | i <- [1..m], j <- [1..n]]
    where
        m = noFilas p
        n = noColumnas p

exponention :: Num a => Array (Int, Int) a -> Int -> Array (Int, Int) a
exponention p n
    | n == 0 = identity (noFilas p)
    | n < 0 = fastExpo (inverso p) -n
    | otherwise = fastExpo p n