module Util where


-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
{-
    IDEA:   agregar adelante del string una cantidad de espacios. La cantidad de espacios se calcula como n
            (el parámetro) menos la longitud del string. Si la longitud de s es mayor a n, el valor queda negativo
            pero replicate entrega lista vacía si se le pide una cantidad cero o negativa
-}
alinearDerecha :: Int -> String -> String
alinearDerecha n s = replicate (n - length s) ' ' ++ s


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
