import Data.List


recr                :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z []         = z
recr f z (x : xs)   = f x xs (recr f z xs)


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

curry'              :: ( (a, b) -> c) -> a -> b -> c
curry' f x y        = f (x, y)

--------------------
-- 2.II
-- Definir la funcion uncurry, que dada una funcion currificada de dos argumentos, devuelve su version no currificada equivalente.
-- Es la inversa de la anterior.
--

uncurry'            :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y)   = f x y


--------------------
-- 2.III
-- Se podría definir una funcion curryN, que tome una funcion de un numero arbitrario de argumentos y devuelva su version currificada?
-- Sugerencia: pensar cual seria el tipo de la funcion.
--

-- NO se puede definir una funcion generica, pero si se puede definir un curry{N} con N especifico (ej: N = 3 seria curry3) que haga
-- el trabajo para esa cantidad de parametros


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

elem_ value = foldr ( \x acc -> acc || x == value) False

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

sumasParciales xs = reverse (foldl step [] xs)
    where
        step []  x = [x]
        step acc x = (x + head acc) : acc
sumasParciales' xs = scanl1 (+) xs


--------------------
-- 3.IV
-- Definir la funcion sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como resultado: el primer elemento,
-- menos el segundo, mas el tercero, menos el cuarto, etc. Usar foldr.
--

sumaAlt xs = fst (foldr (\ x (acc, s) -> (x * s + acc, -s) ) (0, 1) (reverse xs))
sumaAlt' xs = sum (zipWith (*) xs (cycle [1,-1]))

--------------------
-- 3.V
-- Hacer lo mismo que en el punto anterior, pero en sentido inverso (el ultimo elemento menos el anteultimo, etc).
-- Pensar que esquema de recursion conviene usar en este caso.
--

sumaAltRev xs = fst (foldr (\ x (acc, s) -> (x * s + acc, -s) ) (0, 1) xs)
sumaAltRev' xs = sum (zipWith (*) (reverse xs) (cycle [1,-1]))



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
insertarEn e []         = []
insertarEn e (x:xs)     = (permutate e x) ++ insertarEn e xs


permutate :: a -> [a] -> [[a]]
permutate e xs          = insertarAt e xs [ 0 .. (length xs) ]
    where
        insertarAt e xs []      = []
        insertarAt e xs (n:ns)  = ( (take n xs) ++ [e] ++ (drop n xs) ) : insertarAt e xs ns


permutaciones'  :: [a] -> [[a]]
permutaciones'  []      = [[]]
permutaciones'  xs      =
                concatMap (\(i, x) ->
                    let resto = take i xs ++ drop (i+1) xs
                    in map (x:) (permutaciones' resto)
                ) (zip [0..] xs)

permutaciones'' :: [a] -> [[a]]
permutaciones'' []      = [[]]
permutaciones'' xs      = concatMap (\(y,ys) -> map (y:) (permutaciones'' ys)) (choices xs)
    where
        -- función auxiliar: devuelve cada elemento junto con la lista de los demás
        choices :: [a] -> [(a,[a])]
        choices []      = []
        choices (x:xs)  = (x,xs) : [(y, x:ys) | (y,ys) <- choices xs]



--------------------
-- 4.II
-- Definir la función partes, que recibe una lista L y devuelve la lista de todas las listas formadas por los mismos elementos de L,
-- en su mismo orden de aparición.
--
-- Ejemplo: partes [5, 1, 2] -> [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]] (en algún orden).
--


partes :: [a] -> [[a]]
partes xs = recr (\x xs acc -> map (x:) (partes xs) ++ acc) [[]] xs


partes' :: [a] -> [[a]]
partes' [] = [[]]
partes' (x:xs) = map (x:) (partes' xs) ++ partes' xs


--------------------
-- 4.III
-- Definir la función prefijos, que dada una lista, devuelve todos sus prefijos.
-- Ejemplo: prefijos [5, 1, 2] -> [[], [5], [5, 1], [5, 1, 2]]
--

prefijos :: [a] -> [[a]]
prefijos xs = foldr appendPrefix [[]] xs
    where
        appendPrefix x acc = [] : map (x:) acc


--------------------
-- 4.IV
-- Definir la función sublistas que, dada una lista, devuelve todas sus sublistas (listas de elementos que aparecen consecutivos
-- en la lista original).
-- Ejemplo: sublistas [5, 1, 2] -> [[], [5], [1], [2], [5, 1], [1, 2], [5, 1, 2]] (en algún orden).
--


sublistas :: [a] -> [[a]]
sublistas xs = recr (\x xs acc -> map (x:) (prefijos xs) ++ acc) [[]] xs



---------------------------------------
-- EJERCICIO 5

-- Indicar si la recursión utilizada en cada una de las siguientes functiones es o no estructural. Si lo es, reescribirla utilizando foldr.
-- En caso contrario, explicar el motivo.

-- NO ES ESTRUCTURAL, PORQUE se no se puede usar elementosEnPosicionesPares (tail xs), deberia ser elementosEnPosicionesPares xs para ser estructural

elementosEnPosicionesPares          :: [a] -> [a]
elementosEnPosicionesPares []       = []
elementosEnPosicionesPares (x:xs)   = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)


-- SI ES ESTRUCTURAL, EN DOS PARAMETROS. VAMOS A EXCRIBIR PRIMERO LOS 4 CASOS, LUEGO CONVERTIR A FOLDR

entrelazar              :: [a] -> [a] -> [a]
entrelazar []           = id
entrelazar (x:xs)       = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)

entrelazar'                 :: [a] -> [a] -> [a]
entrelazar' []      []      = []
entrelazar' []      ys      = ys
entrelazar' xs      []      = xs
entrelazar' (x:xs)  (y:ys)  = x : y : entrelazar' xs ys

entrelazar''                :: [a] -> [a] -> [a]
entrelazar'' xs ys          = reverse (fst (foldr interlace ([], (xs, ys)) (take ((length xs + length ys) * 2) [ 0 .. ]) ))
    where
        interlace pos (acc, (xs, ys))
            | odd  pos && not (null xs)     = (head xs : acc, (tail xs,      ys))
            | even pos && not (null ys)     = (head ys : acc, (     xs, tail ys))
            | not (null xs)                 = (head xs : acc, (tail xs,      ys))
            | not (null ys)                 = (head ys : acc, (     xs, tail ys))
            | otherwise                     = (          acc, (     [],      []))



---------------------------------------
-- EJERCICIO 6

--
-- El siguiente esquema captura la recursión primitiva sobre listas.
--
-- recr                 :: (a -> [a] -> b -> b) -> b -> [a] -> b
-- recr _ z []          = z
-- recr f z (x : xs)    = f x xs (recr f z xs)
--

--------------------
-- 6.a
-- Definir la funcion sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el resultado de eliminar
-- de la lista la primera aparicion del elemento (si esta presente).
--

sacarUna    :: Eq a => a -> [a] -> [a]
sacarUna e  = recr (\ x xs filtered -> if x == e then xs else x:filtered) []

--------------------
-- 6.b
-- Explicar por que el esquema de recursion estructural (foldr) no es adecuado para implementar la funcion sacarUna del punto anterior.
--
-- El problema surge del hecho de que es necesario sacar SOLO la primera aparicion de la lista, no todas.
-- Por este motivo, cuando el elemento actual es el que se esta buscando, se debe retornar lo que queda de la lista.
-- Esto rompe el principio de la recursion estructural que indica que en el caso x:xs, se puede usar la variable x y se puede usar
-- la cola SIEMPRE que esta asociada a la recursion (en este caso, deberia ser "sacarUna xs") pero para cortar la recursion debemos
-- referenciar xs sin la llamada a la funcion recursiva
--

--------------------
-- 6.c
-- Definr la funcion insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista ordenada (de manera creciente),
-- de manera que se preserva el ordenamiento.

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\ x xs inserted -> if x <= e then x : (if null inserted then [e] else inserted) else e : x : xs) []




---------------------------------------
-- EJERCICIO 7
--
-- Definir las siguientes funciones para trabajar sobre listas, y dar su tipo. Todas ellas deben poder aplicarse a
-- listas finitas e infinitas.

--------------------
-- 7.I
-- mapPares, una version de map que toma una función currificada de dos argumentos y una lista de pares de valores, y devuelve la
-- lista de aplicaciones de la funcion a cada par. Pista: recordar curry y uncurry.
--

mapPares            :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f          = map (uncurry f)


mapPares'           :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares' f []      = []
mapPares' f (x:xs)  = (uncurry f) x : mapPares' f xs





--------------------
-- 7.II
-- armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posicion, el elemento correspondiente a esa posicion en
-- cada una de las listas. Si una de las listas es mas larga que la otra, ignorar los elementos que sobran (el resultado tendra la longitud
-- de la lista más corta). Esta funcion en Haskell se llama zip.
-- Pista: aprovechar la currificación y utilizar evaluación parcial.
--

--- PREGUNTA: a que se refiere con "aprovechar la currificación y utilizar evaluación parcial"

armarPares                          :: [a] -> [b] -> [(a,b)]
armarPares xs ys                    = reverse (fst (foldl zipSecond ([], map (,) xs) ys))
    where
        zipSecond (acc, firsts) y   = if null firsts then (acc, firsts) else ( (head firsts) y : acc, tail firsts)


armarPares'                 :: [a] -> [b] -> [(a,b)]
armarPares' []     _        = []
armarPares' _      []       = []
armarPares' (x:xs) (y:ys)   = (x, y) : armarPares' xs ys


--------------------
-- 7.III
-- mapDoble, una variante de mapPares, que toma una funcion currificada de dos argumentos y dos listas (de igual longitud), y devuelve
-- una lista de aplicaciones de la funcion a cada elemento correspondiente de las dos listas.
-- Esta funcion en Haskell se llama zipWith.

mapDoble                            :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys                    = reverse (fst (foldl zipSecond ([], map f xs) ys))
    where
        zipSecond (acc, firsts) y   = if null firsts then (acc, firsts) else ( (head firsts) y : acc, tail firsts)

mapDoble'                   :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble' f []     _        = []
mapDoble' f _      []       = []
mapDoble' f (x:xs) (y:ys)   = f x y : mapDoble' f xs ys



---------------------------------------
-- EJERCICIO 8


--------------------
-- 8.I
-- Escribir la funcion sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una matriz como la lista de sus filas.
-- Esto quiere decir que cada matriz sera una lista finita de listas finitas, todas de la misma longitud, con elementos enteros.
-- Recordamos que la suma de matrices se define como la suma celda a celda. Asumir que las dos matrices a sumar están bien formadas y
-- tienen las mismas dimensiones.

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))



--------------------
-- 8.II
-- Escribir la funcion trasponer, que, dada una matriz como las del item i, devuelva su traspuesta. Es decir, en la posicion i, j del
-- resultado esta el contenido de la posicion j, i de la matriz original.
-- Notar que si la entrada es una lista de N listas, todas de longitud M, la salida debe tener M listas, todas de longitud N.
--

trasponer       :: [[Int]] -> [[Int]]
trasponer ms    = foldr transposeRow (prepareMatrix ms) ms
    where
        prepareMatrix matrix = if null matrix then [] else replicate (length (head matrix)) []
        transposeRow row transposed = zipWith (:) row transposed

{-
trasponer [ [1,2,3], [4,5,6]]
trasponer [ [1,0,0], [0,1,0], [0,0,1]]
trasponer [ [0,0,1], [0,1,0], [1,1,1]]
trasponer [ [1,2,3], [4,5,6], [7,8,9]]
-}


---------------------------------------
-- EJERCICIO 9

--------------------
-- 9.I
-- Definir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de Haskell (la funcion
-- va a estar definida solo para los enteros mayores o iguales que 0).
--
-- Operar sobre Integer, desde N hacia 0. Solo considerar enteros positivos y el cero
--
-- VAMOS A LLAMAR A LA FUNCION foldIntegerNat

foldIntegerNat :: (Integer -> a) -> (Integer -> a -> a) -> Integer -> a
foldIntegerNat foldZ foldN n
    | n < 0         = error "N no puede ser negativo"
    | otherwise     = _foldIntegerNat foldZ foldN n
    where
        _foldIntegerNat :: (Integer -> a) -> (Integer -> a -> a) -> Integer -> a
        _foldIntegerNat foldZ foldN 0 = foldZ 0
        _foldIntegerNat foldZ foldN n = foldN n (_foldIntegerNat foldZ foldN (n-1))

{-
foldIntegerNat (const 0) (+) 1      = 1
foldIntegerNat (const 1) (*) 1      = 1
foldIntegerNat (const 1) (*) 4      = 24
foldIntegerNat (const 1) (*) 5      = 120
-}


--------------------
-- 9.II
-- Utilizando foldIngegerNat, definir la funcion potencia.

powerIntegerNat :: Integer -> Integer -> Integer
powerIntegerNat base = foldIntegerNat (const 1) (\l r -> base * r)

{-
powerIntegerNat 2 4       = 16
powerIntegerNat 2 32      = 4294967296
powerIntegerNat 2 10      = 1024
powerIntegerNat 2 0       = 1
powerIntegerNat 2 1       = 2
-}




---------------------------------------
-- EJERCICIO 10

--------------------
-- 10.I
-- Definir la funcion genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una cantidad dada de elementos, a partir de
-- un elemento inicial y de una funcion de incremento entre los elementos de la lista. Dicha funcion de incremento, dado un elemento de
-- la lista, devuelve el elemento siguiente.

genLista :: a -> (a -> a) -> Integer -> [a]
genLista start next count = reverse (fst (foldr (genNext) ([start], start) [1..count - 1]))
    where
        genNext count (acc, current) = (next current : acc, next current)



--------------------
-- 10.II
-- Usando genLista, definir la funcion desdeHasta, que dado un par de numeros (el primero menor que el segundo), devuelve una lista de
-- numeros consecutivos desde el primero hasta el segundo.

desdeHasta              :: Integer -> Integer -> [Integer]
desdeHasta start end    = genLista start (1+) (end - start + 1)



---------------------------------------
-- EJERCICIO 11

data Polinomio a =  X
                  | Cte a
                  | Suma (Polinomio a) (Polinomio a)
                  | Prod (Polinomio a) (Polinomio a)

--------------------
-- primera parte
-- Definir el esquema de recursion estructural para Polinomio a

foldPolinomio :: (a -> b) -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> a -> Polinomio a -> b
foldPolinomio fX fCte fSuma fProd x poli = case poli of
        X                       -> fX x
        Cte f                   -> fCte f
        Suma (left) (right)     -> fSuma (recurse left) (recurse right)
        Prod (left) (right)     -> fProd (recurse left) (recurse right)
    where
        recurse e = foldPolinomio fX fCte fSuma fProd x e



--------------------
-- segunda parte
-- Luego usar el esquema definido para escribir la funcion evaluar :: Num a => a -> Polinomio a -> a que, dado un numero y un polinomio,
-- devuelve el resultado de evaluar el polinomio dado en el numero dado.

evaluarPolinomio :: Num a => a -> Polinomio a -> a
evaluarPolinomio x = foldPolinomio id id (+) (*) x

-- evaluarPolinomio 2 (Suma (Prod (Cte 3.5) (Prod (X) (X)) ) (Suma (Prod (Cte 1.2) (X)) (Cte 4)) )




---------------------------------------
-- EJERCICIO 12

data AB a = Nil | Bin (AB a) a (AB a)
    deriving (Show)

--------------------
-- 12.I
-- Usando recursion explícita, definir los esquemas de recursion estructural (foldAB) y primitiva (recAB), y dar sus tipos.

foldAB      :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB fNil fBin (Nil)                  = fNil
foldAB fNil fBin (Bin left root right)  = fBin (recurse left) root (recurse right)
    where
        recurse branch      = foldAB fNil fBin branch


recrAB      :: (AB a -> b) -> (b -> a -> b -> AB a -> b) -> AB a -> b
recrAB fNil fBin (Nil)                  = fNil Nil
recrAB fNil fBin (Bin left root right)  = fBin (recurse left) root (recurse right) me
    where
        recurse branch      = recrAB fNil fBin branch
        me                  = (Bin left root right)



--------------------
-- 12.II
-- Definir las funciones esNil, altura y cantNodos (para esNil puede utilizarse case en lugar de foldAB o recAB).

esNilAB :: AB a -> Bool
esNilAB Nil = True
esNulAB _   = False


alturaAB :: AB a -> Int
alturaAB = foldAB 1 (\ lHeight root rHeight -> 1 + max lHeight rHeight)

-- alturaAB (Bin (Bin (Bin Nil 6 Nil) 9 (Bin (Bin Nil 11 Nil) 12 (Bin Nil 14 Nil)) ) 17 (Bin (Nil) 19 (Nil)))


cantNodosAB :: AB a -> Int
cantNodosAB = foldAB 0 (\ lCount root rCount -> 1 + lCount + rCount)

-- cantNodos (Bin (Bin (Bin Nil 6 Nil) 9 (Bin (Bin Nil 11 Nil) 12 (Bin Nil 14 Nil)) ) 17 (Bin (Nil) 19 (Nil)))


--------------------
-- 12.III
-- Definir la funcion mejorSegún :: (a -> a -> Bool) -> AB a -> a, analoga a la del ejercicio 3, para arboles.
-- Se recomienda definir una funcion auxiliar para comparar la raiz con un posible resultado de la recursion
-- para un arbol que puede o no ser Nil.

mejorSegunAB :: (Maybe a -> Maybe a -> Bool) -> AB a -> Maybe a
mejorSegunAB f = foldAB (Nothing) evalBin
    where
        evalBin i r d =
            if f i (Just r)
                then if f i d
                        then i
                        else d
                else if f (Just r) d
                        then (Just r)
                        else d

mejorSegunAB' :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB' f Nil                     = error "No se aceptan árboles vacíos"
mejorSegunAB' f (Bin left root right)   = recrAB (\me -> root) evalBin (Bin left root right)
    where
        -- eval on Bin constructor
        evalBin i r d (Bin Nil            root Nil)         = r
        evalBin i r d (Bin (Bin _ _ _)    root Nil)         = if f r i then root else i
        evalBin i r d (Bin Nil            root (Bin _ _ _)) = if f r d then root else d
        evalBin i r d (Bin (Bin _ _ _)    root (Bin _ _ _)) = if f r i then if f r d then r else d else if f i d then i else d

-- mejorSegunAB (<) (Bin (Bin (Bin Nil 6 Nil) 9 (Bin (Bin Nil 11 Nil) 12 (Bin Nil 14 Nil)) ) 17 (Bin (Nil) 19 (Nil)))


--------------------
-- 12.IV
-- Definir la funcion esABB :: Ord a => AB a -> Bool que chequea si un arbol es un arbol binario de busqueda. Recordar que, en un arbol
-- binario de busqueda, el valor de un nodo es mayor o igual que los valores que aparecen en el subarbol izquierdo y es estrictamente
-- menor que los valores que aparecen en el subarbol derecho.

esABB :: Ord a => AB a -> Bool
esABB ab                = fst (foldAB (True, Nothing) evalBin ab)
    where
        -- eval on Bin constructor
        evalBin (ib, im) r (rb, rm) = (ib && rb && _ordered im (Just r) rm, _max im (Just r) rm)
        -- check they are ordered (accept last could be nothing)
        -- _ordered :: Maybe a -> Maybe a -> Maybe a -> Bool
        _ordered l r d = l <= r && (d == Nothing || r < d)
        -- decide the max (using maybe)
        -- _max :: Maybe a -> Maybe a -> Maybe a -> Maybe a
        _max Nothing    Nothing     Nothing     = Nothing
        _max Nothing    (Just r)    Nothing     = Just r
        _max (Just i)   (Just r)    Nothing     = Just (max i r)
        _max Nothing    (Just r)    (Just d)    = Just (max r d)
        _max (Just i)   (Just r)    (Just d)    = Just (max (max i r) d)

-- esABB (Bin (Bin (Bin Nil 6 Nil) 9 (Bin (Bin Nil 11 Nil) 12 (Bin Nil 14 Nil)) ) 17 (Bin (Nil) 19 (Nil)))


--------------------
-- 12.V
-- Justificar la eleccion de los esquemas de recursion utilizados para los tres puntos anteriores.

-- para 12.III
--
-- esNilAB      se usa directamente el patron sobre el input ya que solo para el constructor Nil la funcion retorna true
-- alturaAB     se usa foldAB (recursion estructural) porque no es necesario ver la estructura. El propio foldAB invoca
--              funciones diferentes que pueden procesar
-- cantNodosAB  idem alturaAB
--

-- para 12.III
--
-- mejorSegunAB definido con Maybe puede usar recursion estructural, ya que siempre habra valor para calcular (en el peor de los casos,
--              retornara Nothing)
-- mejorSegunAB' utiliza recursion primitiva. Primero detecta arbol vacio y genera un error. Pero para el caso en donde hay nodos, el
--              analisis requiere entender la estructura del nodo que se esta evaluando, para ver como se realizan las comparaciones
--              (ya que un nodo Bin tendra un valor de raiz, pero no necesariamente valor para los hijos)
-- 

-- para 12.IV
--
-- esABB        se utiliza recursion estructural, ya que se puede generar una tupla donde el primer elemento es un valor Bool para indicar si un
--              subarbol es ABB y el segundo es el valor maximo de dicho subarbol. Se asume que Nil es un ABB (True, Nothing) y el caso del
--              constructor Bin se puede analizar mirando solo las tuplas de los hijos y el valor de la raiz
--




---------------------------------------
-- EJERCICIO 13
--
-- Dado el tipo AB a del ejercicio 12:


--------------------
-- 13.I
-- Definir las funciones ramas (caminos desde la raíz hasta las hojas), cantHojas y espejo.

ramasAB :: AB a -> [[a]]
ramasAB = foldAB [] (\i r d -> map (r:) (if (null i) && (null d) then [[]] else i ++ d))

-- ramasAB (Bin (Bin (Bin Nil 6 Nil) 9 (Bin (Bin Nil 11 Nil) 12 (Bin Nil 14 Nil)) ) 17 (Bin (Nil) 19 (Nil)))

cantHojasAB :: AB a -> Int
cantHojasAB = foldAB 0 (\i r d -> if i == 0 && d == 0 then 1 else i + d)

-- cantHojasAB (Bin (Bin (Bin Nil 6 Nil) 9 (Bin (Bin Nil 11 Nil) 12 (Bin Nil 14 Nil)) ) 17 (Bin (Nil) 19 (Nil)))

espejoAB :: AB a -> AB a
espejoAB = foldAB Nil (\i r d -> Bin d r i)

-- espejoAB (Bin (Bin (Bin Nil 6 Nil) 9 (Bin (Bin Nil 11 Nil) 12 (Bin Nil 14 Nil)) ) 17 (Bin (Nil) 19 (Nil)))


--------------------
-- 13.II
-- Definir la función mismaEstructura :: AB a -> AB b -> Bool que, dados dos arboles, indica si estos tienen la misma forma,
-- independientemente del contenido de sus nodos.
-- Pista: usar evaluación parcial y recordar el ejercicio 7.

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura expr1 = foldAB checkNil checkBin expr1
    where
        -- Nil and Bin checkers
        checkNil = \ expr2 -> isNil expr2
        checkBin checkLeft _ checkRight = \ expr2 -> case expr2 of
                                                Nil             -> False
                                                (Bin l2 _ r2)   -> checkLeft l2 && checkRight r2
        -- checks if the Constructor is Nil
        isNil :: AB b -> Bool
        isNil Nil = True
        isNil _ = False
        -- checks if the Constructor is Nin
        isBin :: AB b -> Bool
        isBin (Bin _ _ _) = True
        isBin _ = False

{-
mismaEstructura (Bin (Bin (Bin Nil 6 Nil) 9 (Bin (Bin Nil 11 Nil) 12 (Bin Nil 14 Nil)) ) 17 (Bin (Nil) 19 (Nil))) (Bin (Bin (Bin Nil 'z' Nil) 'j' (Bin (Bin Nil 'a' Nil) 'b' (Bin Nil 'c' Nil)) ) 'd' (Bin (Nil) 'e' (Nil)))
-}


---------------------------------------
-- EJERCICIO 14
--
-- Se desea modelar en Haskell los arboles con informacion en las hojas (y solo en ellas). Para esto introduciremos el siguiente tipo:
--
--      data AIH a = Hoja a | Bin (AIH a) (AIH a)
--

data AIH a = AIHoja a | AIBin (AIH a) (AIH a)
    deriving (Show)

--------------------
-- 14.I
-- Definir el esquema de recursion estructural foldAIH y dar su tipo. Por tratarse del primer esquema de recursipn que tenemos para
-- este tipo, se permite usar recursion explicita.

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH fHoja fBin (AIHoja a)           = fHoja a
foldAIH fHoja fBin (AIBin left right)   = fBin (recurse left) (recurse right)
    where
        recurse branch                  = foldAIH fHoja fBin branch


--------------------
-- 14.I
-- Escribir las funciones
-- altura :: AIH a -> Integer
-- tamaño :: AIH a -> Integer
--
-- Considerar que la altura de una hoja es 1 y el tamaño de un AIH es su cantidad de hojas.
--

altura :: AIH a -> Integer
altura = foldAIH (const 1) (\l r -> 1 + max l r)

tamaño :: AIH a -> Integer
tamaño = foldAIH (const 1) (+)


{-

sampleAIH = AIBin (AIHoja 14) (AIBin (AIBin (AIBin (AIBin (AIHoja (-2)) (AIHoja 7)) (AIHoja 93)) (AIBin (AIHoja 4) (AIHoja 1024))) (AIBin (AIHoja 3) (AIBin (AIHoja 4) (AIHoja 9)) ) )

altura sampleAIH -> 6
tamaño sampleAIH -> 9

-}



---------------------------------------
-- EJERCICIO 15
--


--------------------
-- 15.I
-- Definir el tipo RoseTree de arboles no vacios, con una cantidad indeterminada de hijos para cada nodo.
--

data RoseTree a = RoseNode a [ RoseTree a ]
    deriving (Show)


--------------------
-- 15.II
-- Escribir el esquema de recursion estructural para RoseTree. Importante escribir primero su tipo.

foldRoseTree :: (a -> [b] -> b) -> RoseTree a -> b
foldRoseTree fNode (RoseNode value nodes) = fNode value (map (foldRoseTree fNode) nodes)



--------------------
-- 15.III
-- Usando el esquema definido, escribir las siguientes funciones:

sampleRoseTree = RoseNode 5 [ RoseNode 7 [ RoseNode 29 [] ], RoseNode 35 [ RoseNode 93 [ RoseNode 1008 [], RoseNode 1024 [ RoseNode 1025 [ ] ] ] ], RoseNode 1048 [] ]

--------------------
-- 15.III.a
-- hojas, que dado un RoseTree, devuelva una lista con sus hojas ordenadas de izquierda a derecha, segun su aparicion en el RoseTree.
--

hojasRoseTree :: RoseTree a -> [a]
hojasRoseTree = foldRoseTree listarHojas
    where
        listarHojas :: a -> [[a]] -> [a]
        listarHojas a hojas = a : concat hojas


-- ejercicio de la clase
ramasRoseTree :: RoseTree a -> [[a]]
ramasRoseTree = foldRoseTree (\x rec -> if null rec 
                                    then [[x]]
                                    else map (x:) (concat rec))



--------------------
-- 15.III.b
-- distancias, que dado un RoseTree, devuelva las distancias de su raiz a cada una de sus hojas.
--


distanciasRoseTree :: RoseTree a -> [Int]
distanciasRoseTree = foldRoseTree medirHojas
    where
        medirHojas :: a -> [[Int]] -> [Int]
        medirHojas a hojas = 0 : map (+1) (concat hojas)



--------------------
-- 15.III.c
-- altura, que devuelve la altura de un RoseTree (la cantidad de nodos de la rama mas larga). Si el RoseTree es una hoja, se considera
-- que su altura es 1.
--

alturaRoseTree :: RoseTree a -> Int
alturaRoseTree = foldRoseTree medirAltura
    where
        medirAltura :: a -> [Int] -> Int
        medirAltura a hojas = 1 + maximum (0 : hojas)





---------------------------------------
-- EJERCICIO 16
--
-- Se desea representar conjuntos mediante Hashing abierto (chain addressing). El Hashing abierto consta de dos funciones: una funcion
-- de Hash, que dado un elemento devuelve un valor entero (el cual se espera que no se repita con frecuencia), y una tabla de Hash,
-- que dado un numero entero devuelve los elementos del conjunto a los que la funcion de Hash asigno dicho numero (es decir, la
-- preimagen de la funcion de Hash para ese numero).
--
-- Por contexto de uso, vamos a suponer que la tabla de Hash es una funcion total, que devuelve listas vacías para los numeros que no
-- corresponden a elementos del conjunto. Este es un invariante que debera preservarse en todas las funciones que devuelvan conjuntos.
--
-- Los representaremos en Haskell de la siguiente manera:
--

data HashSet a = Hash (a -> Integer) (Integer -> [a])


--
-- Definir las siguientes funciones:

--------------------
-- 16.I
--
-- vacio :: (a -> Integer) -> HashSet a, que devuelve un conjunto vacio con la funcion de Hash indicada.

hashSetVacio :: (a -> Integer) -> HashSet a
hashSetVacio hash = Hash hash (\_ -> [])


--------------------
-- 16.II
--
-- pertenece :: Eq a => a -> HashSet a -> Bool, que indica si un elemento pertenece a un conjunto. Es decir, si se encuentra en la
-- lista obtenida en la tabla de Hash para el numero correspondiente a la funcion de Hash del elemento.
--
-- Por ejemplo:
--
-- hashSetPertenece 5 $ hashSetAgregar 1 $ hashSetAgregar 2 $ hashSetAgregar 1 $ hashSetVacio (flip mod 5) devuelve False
-- hashSetPertenece 2 $ hashSetAgregar 1 $ hashSetAgregar 2 $ hashSetAgregar 1 $ hashSetVacio (flip mod 5) devuelve True


hashSetPertenece :: Eq a => a -> HashSet a -> Bool
hashSetPertenece e (Hash hash buckets) = elem e (buckets (hash e))



--------------------
-- 16.III
--
-- agregar::Eq a => a -> HashSet a -> HashSet a, que agrega un elemento a un conjunto. Si el elemento ya estaba en el conjunto, se
-- debe devolver el conjunto sin modificaciones.

hashSetAgregar :: Eq a => a -> HashSet a -> HashSet a
hashSetAgregar e (Hash hash buckets) = Hash hash (agregaSiNoExiste e)
    where
        bucket = hash e
        bucketContents = buckets bucket
        agregaSiNoExiste e = if elem e bucketContents
                                then buckets
                                else (\b -> if b == bucket then e:bucketContents else buckets b)



--------------------
-- 16.IV
--
-- interseccion :: Eq a => HashSet a -> HashSet a -> HashSet a que, dados dos conjuntos, devuelve un conjunto con la misma funcion
-- de Hash del primero y con los elementos que pertenecen a ambos conjuntos a la vez.


hashSetInterseccion :: Eq a => HashSet a -> HashSet a -> HashSet a
hashSetInterseccion (Hash hash1 buckets1) (Hash hash2 buckets2) = Hash hash1 (\b -> (buckets1 b) `intersect` (buckets2 b))


-- hs1 = hashSetAgregar 1 $ hashSetAgregar 2 $ hashSetAgregar 3 $ hashSetVacio (flip mod 5)
-- hs2 = hashSetAgregar 1 $ hashSetAgregar 3 $ hashSetAgregar 4 $ hashSetVacio (flip mod 5)
-- hs3 = hashSetInterseccion hs1 hs2
-- hashSetPertenece 1 hs3
-- hashSetPertenece 2 hs3
-- hashSetPertenece 3 hs3
-- hashSetPertenece 4 hs3
-- hashSetPertenece 5 hs3



--------------------
-- 16.V
--
-- foldr1 (no relacionada con los conjuntos). Dar el tipo y definir la funcion foldr1 para listas sin usar recursion explicita,
-- recurriendo a alguno de los esquemas de recursion conocidos.
-- Se recomienda usar la función error :: String -> a para el caso de la lista vacia.

foldr1''' :: (a -> a -> a) -> [a] -> a
foldr1''' _ [] = error "La lista no puede ser vacia"
foldr1''' f xs = foldr f (last xs) (init xs)

-- foldr1''' min [ -1, 0, 1, 2, 3, 4 ]
-- foldr1''' min []






---------------------------------------
-- EJERCICIO 17
--
-- Cual es el valor de esta expresion?
--
-- [ x | x <- [1..3], y <- [x..3], (x + y) mod 3 == 0 ]
--

-- RESPUESTA: [ 1, 3 ]




---------------------------------------
-- EJERCICIO 18
--
-- Definir la lista infinita paresDeNat::[(Int,Int)], que contenga todos los pares de numeros naturales: (0,0), (0,1), (1,0), etc.
--

paresDeNat :: [ (Int, Int) ]
paresDeNat = [ (x,y) | x <- [0..], y <- [0..x]]



---------------------------------------
-- EJERCICIO 19
--
-- Una tripla pitagorica es una tripla (a, b, c) de enteros positivos tal que a2 + b2 = c2.
-- La siguiente expresion intenta ser una definicion de una lista (infinita) de triplas pitagoricas:
--
-- pitagoricas :: [(Integer, Integer, Integer)]
-- pitagoricas = [(a, b, c) | a <- [1..], b <-[1..], c <- [1..], a^2 + b^2 == c^2]
--
-- Explicar por que esta definición no es util. Dar una definición mejor.

pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a, b, c) | a <- [1..], b <-[1..], c <- [1..], a^2 + b^2 == c^2]

-- la definicion no es util porque para el primer par de a,b va a intentar con todos los posibles (infinitos) C antes de seguir

pitagoricas' :: [(Integer, Integer, Integer)]
pitagoricas' = [ (a, b, c) | c <- [1..], a <- [1..c], b <- [1..c], c^2 == a^2 + b^2 ]

-- esta definicion es mejor porque itera los C (el lado mas largo) y tiene una cantidad acotada de a y b que probar, así que retorna
-- valores con mayor frecuencia




---------------------------------------
-- EJERCICIO 20
--
-- Escribir la funcion listasQueSuman :: Int -> [[Int]] que, dado un numero natural n, devuelve todas las listas de enteros positivos
-- (es decir, mayores o iguales que 1) cuya suma sea n. Para este ejercicio se permite usar recursion explicita. Pensar por que la
-- recurson utilizada no es estructural. (Este ejercicio no es de generacion infinita, pero puede ser util para otras funciones que
-- generen listas infinitas de listas).
--


listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n | n > 0 = [x : xs | x <- [1..n], xs <- listasQueSuman (n-x)]


-- HECHOS PENSANDO QUE ERAN LISTAS CON ELEMENTOS ASCENDENTES SOLAMENTE

listasQueSuman' :: Int -> [[Int]]
listasQueSuman' n = [ parte | parte <- partes [1..n], n == sum parte ]
    where
        partes :: [a] -> [[a]]
        partes xs = recr (\x xs acc -> map (x:) (partes xs) ++ acc) [[]] xs


listasQueSuman'' :: Int -> [[Int]]
listasQueSuman'' n = [ lista | lista <- generarListas n ]
    where
        generarListas :: Int -> [[Int]]
        generarListas n = filter (\l -> n == (sum l)) (partes [1..n])
        partes :: [a] -> [[a]]
        partes xs = recr (\x xs acc -> acc ++ map (x:) (partes xs)) [[]] xs




---------------------------------------
-- EJERCICIO 21
--
-- Definir en Haskell una lista que contenga todas las listas finitas de enteros positivos (esto es, con elementos
-- mayores o iguales que 1).


todasLasListas = concatMap listasQueSuman [1..]


todasLasListas' = [] : partesDe 1
    where
        partesDe n = (filter (\l -> if null l || maximum l < n then False else True) (partes [1..n]) ) ++ partesDe (n+1)
        partes :: [a] -> [[a]]
        partes xs = recr (\x xs acc -> acc ++ map (x:) (partes xs)) [[]] xs




---------------------------------------
-- EJERCICIO 22
--
-- Dado el tipo de datos AIH a definido en el ejercicio 14:
--

-- definido para el ejercicio 14
-- data AIH a = AIHoja a | AIBin (AIH a) (AIH a)
--    deriving (Show)


--------------------
-- 22.a
-- Definir la lista (infinita) de todos los AIH cuyas hojas tienen tipo (). Se recomienda definir una función auxiliar. Para este
-- ejercicio se permite utilizar recursión explicita.


cantHojasAIH :: Int -> AIH ()
cantHojasAIH 0 = error "No se permiten arboles nulos"
cantHojasAIH 1 = AIHoja ()
cantHojasAIH n = error "No implementado"

{-
Hoja -> Bin Hoja Hoja -> Bin (Bin Hoja Hoja) Hoja -> Bin Hoja (Bin Hoja Hoja) -> Bin (Bin Hoja Hoja) (Bin Hoja Hoja)


todosLosAIH = generarTodos [AIHHoja ()]
    where
        generarTodos :: [AIHHoja ()] -> [[AIHHoja ()]]
        generarTodos lista = nuevaLista ++ generarTodos nuevaLista
        where
            nuevaLista = expandOneLevel lista
            expandOneLevel :: [[AIHHoja ()]] -> [[AIHHoja ()]]
            expandOneLevel lista = 

-}


--------------------
-- 22.b
-- Explicar por que la recursion utilizada en el punto a) no es estructural.






{-
foldr               :: (a -> b -> b) -> b -> [a] -> b
foldr f z []        = z
foldr f z (x : xs)  = f x (foldr f z xs)

recr                :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z []         = z
recr f z (x : xs)   = f x xs (recr f z xs)

foldl               :: (b -> a -> b) -> b -> [a] -> b
foldl f ac []       = ac
foldl f ac (x : xs) = foldl f (f ac x) xs
-}





---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
--
-- PENDIENTE
--
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------





