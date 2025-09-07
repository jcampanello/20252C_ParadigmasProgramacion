import Data.List

---------------------------------------
-- TEORICA

ultimoIndiceDe :: Eq a => a -> [a] -> Int
ultimoIndiceDe x [] = error "No se puede buscar en cadena vacia"
ultimoIndiceDe x (y:ys)
    | x /= y || elem x ys   = 1 + ultimoIndiceDe x ys
    | otherwise             = 0

ultimoIndiceDe' :: Eq a => a -> [a] -> Maybe Int
ultimoIndiceDe' x []        = Nothing
ultimoIndiceDe' x ys        = encontrarMaxPos x ys [0..]
    where
    encontrarMaxPos :: Eq a => a -> [a] -> [Int] -> Maybe Int
    encontrarMaxPos x [] _          = Nothing
    encontrarMaxPos x (y:ys) (s:ss) = max (if x == y then Just s else Nothing) (encontrarMaxPos x ys ss)


-- ultimoIndiceDe 'b' "abracadabra"
-- ultimoIndiceDe' 'b' "abracadabra"


subsecuencias :: [a] -> [ [a] ]
subsecuencias [] = [ [] ]
subsecuencias (x:xs) = duplicarConYSin x (subsecuencias xs)
    where
        duplicarConYSin :: a -> [ [a] ] -> [ [a] ]
        duplicarConYSin x []        = []
        duplicarConYSin x (l:ls)    = (x:l):l:duplicarConYSin x ls



---------------------------------------
-- EJERCICIO 1

--
-- PEND
--


---------------------------------------
-- EJERCICIO 2

--------------------
-- 2.a
-- valorAbsoluto :: Float -> Float, que dado un numero devuelve su valor absoluto.
--
valorAbsoluto :: Float -> Float
valorAbsoluto f = 
    if f >= 0
        then f
        else (-f)

--------------------
-- 2.b
-- bisiesto :: Int -> Bool, que dado un numero que representa un ano, indica si el mismo es bisiesto.
--
bisiesto :: Int -> Bool
bisiesto y = (mod y 4 == 0) && ( (mod y 100 /= 0) || (mod y 400 == 0) )

--------------------
-- 2.c
-- factorial :: Int -> Int, definida unicamente para enteros positivos, que computa el factorial.
--
factorial :: Int -> Int
factorial n
    | n < 0     = error "Input cannot be negative!"
    | n == 0    = 1
    | otherwise = n * factorial (n - 1)

--------------------
-- 2.d
-- cantDivisoresPrimos :: Int -> Int, que dado un entero positivo devuelve la cantidad de divisores primos.
--
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n
    | n <= 0     = error "Input must be a positive integer"
    | n == 1     = 0
    | otherwise  = length (divisoresPrimos n)

divisoresPrimos :: Int -> [Int]
divisoresPrimos n
    | n <= 0     = error "Input must be a positive integer"
    | n == 1     = []
    | otherwise  = foldr (\prime divisores -> if mod n prime == 0 then (prime:divisores) else divisores) [] (generatePrimes n)


generatePrimes :: Int -> [Int]
generatePrimes 0 = []
generatePrimes 1 = []
generatePrimes n = findPrimes [2 .. n] []
    where
        findPrimes :: [Int] -> [Int] -> [Int]
        findPrimes [] primes = primes
        findPrimes (n : ns) primes =
            if  foldr (\d acc -> acc || mod n d == 0) False primes
                then findPrimes ns primes
                else findPrimes ns (n : primes)



cantDivisoresPrimos_ :: Int -> Int
cantDivisoresPrimos_ n
    | n <= 0     = error "Input must be a positive integer"
    | otherwise  = length (nub (factorizar n))

factorizar :: Int -> [Int]
factorizar n = divisoresPrimos n 2
    where
        divisoresPrimos :: Int -> Int -> [Int]
        divisoresPrimos 0 _ = []
        divisoresPrimos 1 _ = []
        divisoresPrimos n factor =
            if mod n factor == 0
                then factor : divisoresPrimos (div n factor) factor
                else divisoresPrimos n (factor + 1)



---------------------------------------
-- EJERCICIO 3

--------------------
-- 3.a
-- Definir la funcion inverso :: Float → Maybe Float
--
inverso :: Float -> Maybe Float
inverso 0.0 = Nothing
inverso f = Just (1 / f)


--------------------
-- 3.b
-- Definir la funcion aEntero :: Either Int Bool → Int que convierte a entero una expresion que puede ser booleana o entera
--
aEntero :: Either Int Bool -> Int
aEntero (Right False) = 0
aEntero (Right True) = 1
aEntero (Left n) = n


---------------------------------------
-- EJERCICIO 4

--------------------
-- 4.a
-- limpiar :: String → String -> String, que elimina todas las apariciones de cualquier caracter de la primera cadena en la segunda.
--
limpiar :: String -> String -> String
limpiar [] _ = []
limpiar _ [] = []
limpiar toFilter (s:xs) =
    if  foldr (\filtering acc -> acc || filtering == s) False toFilter
        then limpiar toFilter xs
        else s : limpiar toFilter xs

--------------------
-- 4.b
-- difPromedio :: [Float] -> [Float] que dada una lista de numeros devuelve la diferencia de cada uno con el promedio general
--
difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio fs = foldr (\f acc -> (f - (average fs)) : acc ) [] fs

average :: [Float] -> Float
average [] = 0
average fs = (sum fs) / fromIntegral (length fs)


--------------------
-- 4.c
-- todosIguales :: [Int] -> Bool que indica si una lista de enteros tiene todos sus elementos iguales
--
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (n:[]) = True
todosIguales (n:m:ns) = n == m && todosIguales (m:ns)



---------------------------------------
-- EJERCICIO 5

data AB a = Nil | Bin (AB a) a (AB a)
    deriving Show

--------------------
-- 5.a
-- vacioAB :: AB a -> Bool que indica si un arbol es vacio (i.e. no tiene nodos).
--
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

--------------------
-- 5.b
-- negacionAB :: AB Bool -> AB Bool que dado un arbol de booleanos construye otro formado por la negacion de cada uno de los nodos
--
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin left root right) = Bin (negacionAB left) (not root) (negacionAB right)


--------------------
-- 5.c
-- productoAB :: AB Int -> Int que calcula el producto de todos los nodos del arbol.
--
productoAB :: AB Int -> Int
productoAB Nil                      = 0
productoAB (Bin Nil  root Nil)      = root
productoAB (Bin Nil  root right)    = root * productoAB right
productoAB (Bin left root Nil)      = root * productoAB left
productoAB (Bin left root right)    = root * (productoAB left) * (productoAB right)




