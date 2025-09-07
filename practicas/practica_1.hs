import Data.List



---------------------------------------
-- EJERCICIO 1
--
-- complete the type of the functions

-- max2         :: Ord a => (a, a) -> a
max2 (x, y)
    | x >= y    = x
    | otherwise = y

-- evaluarEnCero :: (a -> Int -> b) -> (a -> b)
evaluarEnCero   = \ f -> f 0

-- dosVeces     :: (a -> a) -> (a -> a)
dosVeces        = \ f -> f . f

-- normaVectorial :: (a, a) -> a
normaVectorial (x, y) = sqrt (x ^ 2 + y ^ 2)

-- flipAll      :: [ a -> b -> c ] -> [ b -> a -> c ]
flipAll         = map flip

-- subtract2    :: Num a -> a -> a -> a
subtract2       = flip (-)

-- flipRaro     :: b -> (a -> b -> c) -> a -> c
flipRaro        = flip flip

-- predecesor   :: Num a => a => a
predecesor      = subtract 1


-- PREGUNTA: porque no le gusta que ponga el tipo primero (ej: en subtract2)


---------------------------------------
-- EJERCICIO 2

--------------------
-- 2.I
-- Definir la funcion curry, que dada una funcion de dos argumentos, devuelve su equivalente currificada.
--

-- curry        :: ( (a, b) -> c) -> a -> b -> c
curry f x y     = f (x, y)

--------------------
-- 2.II
-- Definir la funcion uncurry, que dada una funcion currificada de dos argumentos, devuelve su version no currificada equivalente.
-- Es la inversa de la anterior.
--

-- uncurry      :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y


--------------------
-- 2.III
-- Se podría definir una funcion curryN, que tome una funcion de un numero arbitrario de argumentos y devuelva su version currificada?
-- Sugerencia: pensar cual seria el tipo de la funcion.
--

-- NO, se puede definir un curryN con N especifico (ej: N = 3 seria curry3) que haga el trabajo para esa cantidad de parametros


---------------------------------------
-- EJERCICIO 3


--------------------
-- 3.I
-- Redefinir usando foldr las funciones sum, elem, (++), filter y map.
--

{-
foldr               :: (a -> b -> b) -> b -> [a] -> b
foldr f z []        = z
foldr f z (x : xs)  = f x (foldr f z xs)
-}

sum_ xs = foldr (+) 0 xs

elem_ value = foldr ( \x acc -> x == value || acc) False

(+++) xs ys = foldr (:) ys xs

filter_ p = foldr (\ x acc -> if p x then x : acc else acc) []

map_ f = foldr ( (:) . f) []


--------------------
-- 3.II
-- Definir la función mejorSegun :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento de la lista segin una funcion de comparacion,
-- utilizando foldr1. Por ejemplo, maximum = mejorSegun (>).
--

mejorSegun f = foldr1 (\ x y -> if f x y then x else y)


--------------------
-- 3.III
-- Definir la funcion sumasParciales :: Num a => [a] -> [a], que dada una lista de numeros devuelve otra de la misma longitud, que tiene
-- en cada posicion la suma parcial de los elementos de la lista original desde la cabeza hasta la posicion actual.
-- Por ejemplo, sumasParciales [1,4,-1,0,5] = [1,5,4,4,9]
--

{-
recr                :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z []         = z
recr f z (x : xs)   = f x xs (recr f z xs)

foldl               :: (b -> a -> b) -> b -> [a] -> b
foldl f ac []       = ac
foldl f ac (x : xs) = foldl f (f ac x) xs
-}

sumasParciales xs = reverse (foldl step [] xs)
    where
        step []  x = [x]
        step acc x = (x + head acc) : acc


--------------------
-- 3.IV
-- Definir la funcion sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como resultado: el primer elemento,
-- menos el segundo, mas el tercero, menos el cuarto, etc. Usar foldr.
--

sumaAlt xs = fst (foldr (\ x (acc, s) -> (x * s + acc, -s) ) (0, 1) (reverse xs))

--------------------
-- 3.V
-- Hacer lo mismo que en el punto anterior, pero en sentido inverso (el ultimo elemento menos el anteultimo, etc).
-- Pensar que esquema de recursion conviene usar en este caso.
--

sumaAltRev xs = fst (foldr (\ x (acc, s) -> (x * s + acc, -s) ) (0, 1) xs)



---------------------------------------
-- EJERCICIO 4

--------------------
-- 4.I
-- Definir la funcion permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutaciones. Se recomienda utilizar
-- concatMap :: (a -> [b]) -> [a] -> [b], y tambien take y drop.
--

permutaciones   :: [a] -> [[a]]
permutaciones   []      = []
permutaciones   [x]     = [ [x] ]
permutaciones   (x:xs)  = insertarEn x (permutaciones xs)

insertarEn :: a -> [[a]] -> [[a]]
insertarEn e []      = []
insertarEn e (x:xs)  = (permutate e x) ++ insertarEn e xs


permutate :: a -> [a] -> [[a]]
permutate e xs = insertarAt e xs [ 0 .. (length xs) ]
    where
        insertarAt e xs []      = []
        insertarAt e xs (n:ns)  = ( (take n xs) ++ [e] ++ (drop n xs) ) : insertarAt e xs ns


{-
--- combinaciones totales de N elementos en 1, 2 o 3
permutaciones   :: [a] -> [[a]]
permutaciones   [] = []
permutaciones   (x:xs) = insertarEn x (permutaciones xs)

insertarEn :: a -> [[a]] -> [[a]]
insertarEn e []      = [[e]]
insertarEn e (x:xs)  = (permutate e x) ++ insertarEn e xs

permutate :: a -> [a] -> [[a]]
permutate e xs = insertarAt e xs [ -1 .. length xs ]
    where
        insertarAt e xs [] = []
        insertarAt e xs (n:ns) = (if n == -1 then xs else (take n xs) ++ [e] ++ (drop n xs)) : insertarAt e xs ns
-}









