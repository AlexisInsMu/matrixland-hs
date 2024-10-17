--Se colocan todas las funciones que van a ser nombradas en el main_terminal (archivo principal)
module Operations.Operaciones_matrix
    ( prodEscalar
    , multiMatriz
    , identMatriz
    , exponentation
    , listToArray
    ) where

--Librería para los array.
import Data.Array

-- Función para calcular el producto escalar de dos vectores
-- La función va a recibir dos arreglos y va a regresa un 
--valor númerico.
--zip va a junta en tuplas un elemento de un arreglo con uno del otro
--en orden, va a multiplicar esos elemento y va a ir sumando las
--multiplicaciones de cada tupla.
prodEscalar :: Num a => Array Int a -> Array Int a -> a 
prodEscalar x y =
    sum [i * j | (i, j) <- zip (elems x) (elems y)]

-- Función para multiplicar matrices
--Pide 2 arreglos bidimensionales y va a regresar un arreglo bidimensional.
--Primero hacemos un nuevo array con m para el indice max de filas y n para el indice
--max de columnas, donde m se toma de la primera matriz (p) y n se toma de la segunda
--matriz q. Esto se define en el where.
-- Después i y j van a tomar los valores de los indices de 1-m/1-n.
--Luego se va a realizar un producto escalar entre la fila con el primer indice de la
--primera matriz con el primer elemento de la columna de la segunda matriz, así hasta terminar
--con esa fila y columna, y el resultado se guarda en tuplas de i y j.
--Se va a realizar esto continuamente hasta terminar con todos los indices de las dos matrices.
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

-- Funciones auxiliares para obtener filas y columnas--
-- Función para obtener una fila de la matriz
-- Va a recibir un entero (indice de la fila que queremos extraer)
--, luego pide un arreglo bidimensional donde cada elemento tiene
-- un índice (Int, Int) para la posición en la matriz.
-- Y envia un array con la fila deseada.
--ixmap (crea un arreglo a partir de otro), los indices van de 1-n
-- y n es el num de columnas de la fila.
--En la función lambda, transforma un indice del nuevo arreglo (j)
-- a i (indice de la fila), y j (indice de la columna) de la matriz original.
-- bounds va a dar los límite de la matriz.
fila :: Int -> Array (Int, Int) a -> Array Int a
fila i p = ixmap (1, n) (\j -> (i, j)) p
    where (_, (m, n)) = bounds p

-- Función para obtener una columna de la matriz.
-- Se aplica lo mismo que en la función fila, pero ahora para las columnas.
columna :: Int -> Array (Int, Int) a -> Array Int a
columna j q = ixmap (1, m) (\i -> (i, j)) q
    where (_, (m, n)) = bounds q

-- Función para obtener el número de filas
--Pide un arreglo bidimensional donde sus indices va a ser tuplas de enteros.
-- Regresa un entero.
--bounds nos da el rango de índices que se pueden ocupar en el arreglo.
--En la tupla de devuelve bound, ignoramos el primer elemento, y el segundo va
--va a ser un tupla con el indice max de fila (m) y el índice max de columna (n).
--Como queremos el noFila la función devuelve m.
noFilas :: Array (Int, Int) a -> Int
noFilas p = m
    where (_, (m, _)) = bounds p

-- Función para obtener el número de columnas.
-- Se aplica lo mismo que en la función noFila, pero ahora para las columnas.
--Es decir, ahora vamos a trabajar con n (indice max de columnas)
noColumnas :: Array (Int, Int) a -> Int
noColumnas q = n
    where (_, (_, n)) = bounds q

{-- Potenciación de matrices --}
 -- Función para obetner el inverso de una matriz
inverso :: Num a => Array (Int, Int) a -> Array (Int, Int) a
inverso p = array ((1, 1), (m, n)) [((i, j),- (p ! (i, j))) | i <- [1..m], j <- [1..n]]
    where
        m = noFilas p
        n = noColumnas p

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

exponention :: Num a => Array (Int, Int) a -> Int -> Array (Int, Int) a
exponention p n
exponentation :: Num a => Array (Int, Int) a -> Int -> Array (Int, Int) a
exponentation p n
    | n == 0 = listToArray (identMatriz (noFilas p))
    | n < 0 = fastExpo (inverso p) (-n)
    | otherwise = fastExpo p n

-- Función para convertir de una listas de listas a un arreglo
--Recibe una listas de listas y va a mandas un arreglo bidimensional con tuplas de enteros
--como indices.
--Se crea un arreglo con indices de 1-n, rows ocupa length para obtenr el #filas de la lista de listas.
--mientras que cols obtiene el #columnas que el la longitud de la 1ra. sublista de la lista 
--Donde vamos a acceder al elemento que este en i y j de la lista.
--Por ejemplos:
--Si queremos obtener el primer valor de la primera lista, vamos a tener:
-- xss !! 0 !! 0, ya que lo buscamos en i y j = 0.
listToArray :: Num a => [[a]] -> Array (Int, Int) a
listToArray xss = array ((0, 0), (rows - 1, cols - 1)) [((i, j), xss !! i !! j) | i <- [0..rows-1], j <- [0..cols-1]]
  where
    rows = length xss
    cols = length (head xss)

-- Función para convertir un arreglo en una listas de listas.
-- Recibe un arreglo bidimensional con tuplas de enteros como indices, y devuelve una listas de listas.
-- i y j van a tomar valores de 1-m/1-n.
-- m guardar el #filas del arreglo y n el #columnas.
-- ! se usa para acceder al elemento del arreglo en la posición i y j.
arrayToList :: Array (Int, Int) Int -> [[Int]]
arrayToList arr = [[arr ! (i, j) | j <- [0..n-1]] | i <- [0..m-1]]
  where
    m = noFilas arr
    n = noColumnas arr
