-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util ( actualizarElem, infinitoNegativo, infinitoPositivo )
import Data.List (zipWith4)

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)


-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Requiere que @l < u@ y @n >= 1@.
{-
    IDEA:   Se usa el constructor haciendo la cuenta del "ancho" de cada "balde" y crear ademas la lista
            de valores, que es una lista con n+2 ceros (n la cantidad de baldes indicada y 2 adicionales
            para el +/- infinito)
-}
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l  ((u - l) / fromIntegral n)  (replicate (n+2) 0)


-- | Agrega un valor al histograma.
{-
    IDEA:   identificar el "balde" en el cual deberia caer el valor (-Inf,0..n,+Inf) y utilizar la función
            actualizarElem, pasando (+1) como función de actualizacion.
            Para esto, se utiliza una funcion local indiceBalde, que identifica el número de "balde" comparando
            el valor a agregar (x) contra el límite inferior y superior de los baldes definidos (para ir a
            -Inf o +Inf) y sino, con una cuenta identifica el número de balde a usar
-}
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma inicio anchoBalde valores) = Histograma inicio anchoBalde (actualizarElem indiceBalde (+1) valores)
    where
        cantidadBaldes = length (drop 2 valores)
        limiteSuperior = inicio + anchoBalde * fromIntegral cantidadBaldes
        indiceBalde
            | x < inicio            = 0
            | x >= limiteSuperior   = cantidadBaldes + 1
            | otherwise             = 1 + floor ((x - inicio) / anchoBalde)


-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
{-
    IDEA:   Se usa foldr para procesar la lista de números. Para cada número, la función agregar (definida mas arriba)
            es responsable de agregar el número al histograma, que es el acumulador de foldr. El histograma (acumulador)
            se define utilizando la función vacío (definida más arriba) que inicializa el histograma con los atributos n
            lower/upper pasados a esta función

            Se usa foldr porque en este caso, el histograma resultado es el mismo independientemente del orden
            en que se insertan los valores y hecho así, se puede usar directamente la funcion agregar, en lugar de
            necesitar (flip agregar) si se utilizara foldl
-}
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n range = foldr agregar (vacio n range)


-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p


-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
{-
    IDEA:   se usa zipWith4 (recibe 4 listas y pasa 4 parámetros a la función de "unión") con
            Casillero (el constructor) como función de armado. Las listas contienen: el inicio de rango,
            el fin de rango, los valores (parámetro) y los porcentajes (mapeada desde valores).
            Las listas de rangos de inicio y fin, se arman usando n-1 (n = length valores del histograma)
            y el punto del rango (infinito negativo para inicio al comienzo e infinito positivo para el
            rango de fin al final)
-}
casilleros :: Histograma -> [Casillero]
casilleros (Histograma inicio anchoBalde valores) = zipWith4 Casillero rangosInicio rangosFin valores porcentajes
    where
        -- cantidades de baldes y de baldes no borde
        nroTotalBaldes = length valores
        nroBaldes = nroTotalBaldes - 2
        -- limites de n-1 baldes calculados con el rango
        limitesBaldes = [ inicio + (anchoBalde * fromIntegral nro) | nro <- [0 .. nroBaldes] ]
        -- relacionadas con el porcentaje de cada balde del histograma
        cantTotalValores = fromIntegral (sum valores)
        porcentajeBalde cantidad = if cantTotalValores == 0 then 0::Float else (cantidad / cantTotalValores) * 100.0
        -- rangos de inicio y fin de los casilleros
        rangosInicio = infinitoNegativo : limitesBaldes
        rangosFin = limitesBaldes ++ [infinitoPositivo]
        porcentajes = map (porcentajeBalde . fromIntegral) valores
