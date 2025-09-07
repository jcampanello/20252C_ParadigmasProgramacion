module Util where


-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
{-
    IDEA:   la idea es procesar la lista con foldr, generando una tupla como resultado. Al comenzar, el primer
            elemento está vacio y el segundo elemento tiene un cadena de N espacios.
            Por cada elemento de la cadena de input, se acumula en el primer elemento de la tupla y se elimina
            el primer espacio del segundo elemento (salvo que este sea la cadena vacía, en cuyo caso el resultado
            no tendrá padding a izquierda).
            Al finalizar, se concatenan el segundo elemento y el primero (el segundo elemento contendra el
            padding necesario si hiciera falta)

    ACLARACION: es obvio que esto se puede resolver con un IF sobre la longitud de la cadena, pero el
                enunciado dice "No se permite recursion explicita" haciendo entender que se desea el uso
                de foldr, foldl o recr
-}
alinearDerecha :: Int -> String -> String
alinearDerecha n s = snd padded ++ fst padded
    where
        padded = foldr accumAndTrim ( "", take n (repeat ' ') ) s
            where
                accumAndTrim :: Char -> (String, String) -> (String, String)
                accumAndTrim c (acc, "")   = (c : acc, "")
                accumAndTrim c (acc, s:ss) = (c : acc, ss)


-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
{-
    IDEA:   juntar dos listas (la primera los valores y la segunda la secuencia de índices comenzando en cero)
            para poder decidir si N es el índice correcto y asi aplicar la función sobre el valor, caso contrario
            retornar el valor sin cambios.
            Notar que esto protege de índices menores a cero (la segunda lista nunca va a matchear) y de índices
            fuera del rango superior (no habrá mas valores) y esto ocurre por como funciona zipWith
-}
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = zipWith (\ x idx -> if idx == n then f x else x) xs [0..]


-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
