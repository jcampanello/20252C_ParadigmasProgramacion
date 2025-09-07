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
    IDEA:   lo que hay que hacer es simple. Basicamente usar el constructor haciendo la cuenta del
            "ancho" de cada "balde" y crear ademas la lista de valores, que es una lista con n+2 ceros
            (n siendo la cantidad de baldes y 2 adicionales para el +/- infinito)
-}
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l  ((u - l) / fromIntegral n)  (replicate (n+2) 0)


-- | Agrega un valor al histograma.
{-
    IDEA:   identificar el "balde" en el cual deberia caer el valor (-Inf,0..n,+Inf) y utilizar la funcion
            actualizarElem, pasando (+1) como funcion de actualizacion.
            Para esto, se utiliza una funcion local bucketIndex, que identifica el numero de "balde" comparando
            el valor a agregar (x) contra el limite inferior y superior de los baldes definidos (para ir a
            -Inf o +Inf) y sino, con una cuenta identifica el numero de balde a usar
-}
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma first size values) = Histograma first size (actualizarElem bucketIndex (+1) values)
    where
        bucketCount = length (drop 2 values)
        bucketIndex
            | x < first                                         = 0
            | x >= (first + size * fromIntegral bucketCount)    = bucketCount + 1
            | otherwise                                         = 1 + floor ((x - first) / size)


-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
{-
    IDEA:   la funcion es muy simple. Se usara foldr para procesar la lista de numeros. Para cada numero,
            la funcion agregar (definida mas arriba) sera responsable de agregar el numero al histograma,
            que es el acumulador de foldr. El histograma (acumulador) se define utilizando la funcion
            vacio (definida mas arriba) que inicializa el histograma con los atributos n lower/upper pasados
            a esta funcion
-}
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n (lower, upper) values = foldr agregar (vacio n (lower, upper)) values


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
    IDEA:   se usa zipWith4 (recibe 4 listas y pasa 4 parametros a la funcion de construccion) con
            Casillero (el constructor) como funcion de armado. Las listas contienen: el inicio de rango,
            el fin de rango, los valores (parametro) y los porcentajes (mapeada desde valores).
            Las listas de rangos de inicio y fin, se arman usando n-1 (n = length valores del histograma)
            y el punto del rango (infinito negativo para inicio al comienzo e infinito positivo para el
            rango de fin al final)
-}
casilleros :: Histograma -> [Casillero]
casilleros (Histograma first size values) = zipWith4 Casillero rangosInicio rangosFin values (map (percentage . fromIntegral) values)
    where
        nroTotalBaldes = length values
        nroBaldes = nroTotalBaldes - 2
        rangosInicio = infinitoNegativo : [ first + (size * fromIntegral nro) | nro <- [0 .. nroBaldes] ]
        rangosFin = [ first + (size * fromIntegral nro) | nro <- [0 .. nroBaldes] ] ++ [infinitoPositivo]
        cantTotalValores = fromIntegral (sum values)
        percentage cantidad = if cantTotalValores == 0 then 0::Float else (cantidad / cantTotalValores) * 100.0
