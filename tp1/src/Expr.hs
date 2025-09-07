module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

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

-- | @recrExpr fConst fRango fSuma fResta fMult fDiv expr@ procesa @expr@ utilizando el esquema de Recursión Primitiva,
-- donde @fConst@, @fRango@, @fSuma@, @fResta@, @fMult@ y @fDiv@ son las funciones específicas para procesar cada
-- constructor de Expr. Todas estas funciones reciben los mismos parámetros que el constructor con el agregado de
-- un parámetro de tipo Expr, que es la propia expresión que está siendo evaluada (esto es en general de importancia
-- para las funciones @fSuma@, @fResta@, @fMult@ y @fDiv@)
{-
    IDEA:   se implementa el esquema de Recursión Primitiva para el cual se requieren N funciones (una por cada constructor
            disponible en el tipo Expr). Estas funciones toman no solo los atributos de dicho constructor, sino que además
            toman la propia expr que está siendo evaluada, dado que la recursión primitiva permite "ver" el resto de la
            información en cada punto de procesamiento
-}
recrExpr :: (Float -> Expr -> a) -> (Float -> Float -> Expr -> a) -> (a -> a -> Expr -> a) -> (a -> a -> Expr -> a) -> (a -> a -> Expr -> a) -> (a -> a -> Expr -> a) -> Expr -> a
recrExpr fConst fRango fSuma fResta fMult fDiv expr = case expr of
        Const f             -> fConst f expr
        Rango s e           -> fRango e e expr
        Suma expr1 expr2    -> fSuma  (recurse expr1) (recurse expr2) expr
        Resta expr1 expr2   -> fResta (recurse expr1) (recurse expr2) expr
        Mult expr1 expr2    -> fMult  (recurse expr1) (recurse expr2) expr
        Div expr1 expr2     -> fDiv   (recurse expr1) (recurse expr2) expr
  where
      recurse e = recrExpr fConst fRango fSuma fResta fMult fDiv e

-- | @recrExpr fConst fRango fSuma fResta fMult fDiv expr@ procesa @expr@ utilizando el esquema de Recursión Estructural,
-- donde @fConst@, @fRango@, @fSuma@, @fResta@, @fMult@ y @fDiv@ son las funciones específicas para procesar cada
-- constructor de Expr
{-
    IDEA:   se implementa el esquema de Recursión Estructural para el cual se requieren N funciones (una por cada constructor
            disponible en el tipo Expr). Estas funciones toman los atributos de dicho constructor
-}
foldExpr :: (Float -> a) -> (Float -> Float -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr fConst fRango fSuma fResta fMult fDiv expr = case expr of
        Const f             -> fConst f
        Rango s e           -> fRango e e
        Suma expr1 expr2    -> fSuma  (recurse expr1) (recurse expr2)
        Resta expr1 expr2   -> fResta (recurse expr1) (recurse expr2)
        Mult expr1 expr2    -> fMult  (recurse expr1) (recurse expr2)
        Div expr1 expr2     -> fDiv   (recurse expr1) (recurse expr2)
  where
      recurse e = foldExpr fConst fRango fSuma fResta fMult fDiv e


-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval = error "COMPLETAR EJERCICIO 8"

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = error "COMPLETAR EJERCICIO 9"

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = error "COMPLETAR EJERCICIO 10"

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
