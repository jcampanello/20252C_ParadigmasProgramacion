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

import Util ( infinitoNegativo, infinitoPositivo )
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
        Rango s e           -> fRango s e expr
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
        Rango s e           -> fRango s e
        Suma expr1 expr2    -> fSuma  (recurse expr1) (recurse expr2)
        Resta expr1 expr2   -> fResta (recurse expr1) (recurse expr2)
        Mult expr1 expr2    -> fMult  (recurse expr1) (recurse expr2)
        Div expr1 expr2     -> fDiv   (recurse expr1) (recurse expr2)
  where
      recurse e = foldExpr fConst fRango fSuma fResta fMult fDiv e


-- | Evaluar expresiones dado un generador de números aleatorios
{-
    IDEA:     se procesa el árbol representado por la expresión, de forma que para cada nodo se arma una función
              (currificada) que resuelve la operación pero a la cual le queda el parámetro generador pendiente
              (de ahí la currificación). Hay 4 tipos de función generada, asociada a los tipos de nodos de la
              expresión:
                - const (retorna un número constante)
                - rango (retorna dameUno con el rango, pero que no queda evaluada por faltar el generador)
                - operador binario (para +, - y *) que toma dos operandos (left y right) y aplica, pasando el
                  generador por el primero operando y el generador actualizado al segundo
                - operador binario con saturación a infinito (para /). Similar al anterior, pero se evalua que
                  si el segundo operando es 0 => se retorna +/- Infinito según el signo del primer operando.
                  ESTE CASO es importante para lograr que Div sea una función total
-}
eval :: Expr -> G Float
eval expr = foldExpr fConst fRango fSuma fResta fMult fDiv expr
    where
        -- las funciones directas en uso
        fConst f = _const f
        fRango s e = dameUno (s, e)
        fSuma l r = _binOper (+) l r
        fResta l r = _binOper (-) l r
        fMult l r = _binOper (*) l r
        fDiv l r = _binOperWithInf (/) l r
        -- auxiliares para Const y Suma, Resta y Mult
        _const f gen = (f, gen)
        _binOper :: (Float -> Float -> Float) -> G Float -> G Float -> G Float
        _binOper op leftOp rightOp gen = (op leftVal rightVal, genR)
            where
                (leftVal, genL)  = leftOp gen
                (rightVal, genR) = rightOp genL
        -- auxiliares para operadores binarios con saturacion a infinito
        _binOperWithInf :: (Float -> Float -> Float) -> G Float -> G Float -> G Float
        _binOperWithInf op leftOp rightOp gen
                | rightVal == 0   = (if leftVal < 0 then infinitoNegativo else infinitoPositivo, genR)
                | otherwise       = (op leftVal rightVal, genR)
            where
                (leftVal, genL)  = leftOp gen
                (rightVal, genR) = rightOp genL


-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
{-
    IDEA:   se evalúan dos funciones (muestra y rango95), que retornarán datos parciales necesarios
            para crear el histograma.
            La función muestra permite obtener una muestra de N números.
            Esta muestra es luego pasada a la función rango95 que calcula el rango de 95% de confianza (lower y upper)
            que permitirá definir los casilleros del histograma
-}
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m (lowerRange, upperRange) sampleValues, updatedF)
    where
        (sampleValues, updatedF) = muestra f n g
        (lowerRange, upperRange) = rango95 sampleValues



-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
{-
    IDEA:   esta función es en general un simple wrapper a armarHistograma, solo que recibe una expresión en lugar de
            f y dicha expresión se convierte a función realizando una evaluación parcial
-}
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)



-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
--mostrar = error "COMPLETAR EJERCICIO 11"
mostrar expr = recrExpr fConst fRango fSuma fResta fMult fDiv expr
    where
        -- casos simples - constante y rango
        fConst f me            = show f
        fRango s e me          = show s ++ "~" ++ show e
        -- casos sin reduccion de parentesis
        fResta  expr1 expr2 me = binOper " - " expr1 expr2 me
        fDiv    expr1 expr2 me = binOper " / " expr1 expr2 me
        -- casos con reduccion de parentesis
        fSuma   expr1 expr2 me = binOper " + " expr1 expr2 me
        fMult   expr1 expr2 me = binOper " * " expr1 expr2 me
        -- resolucion general para operadores binarios
        binOper :: String -> String -> String -> Expr -> String
        binOper sep expr1 expr2 me = maybeParen (_cuandoLeft me) expr1 ++ sep ++ maybeParen (_cuandoRight me) expr2
        -- decidimos cuando no hacen falta parentesis (para el operador derecho e izquierdo)
        _cuandoLeft :: Expr -> Bool
        _cuandoLeft (Suma leftOp _)     = _cuando CESuma  (constructor leftOp)
        _cuandoLeft (Resta leftOp _)    = _cuando CEResta (constructor leftOp)
        _cuandoLeft (Mult leftOp _)     = _cuando CEMult  (constructor leftOp)
        _cuandoLeft (Div leftOp _)      = _cuando CEDiv   (constructor leftOp)
        _cuandoLeft _                   = False
        _cuandoRight :: Expr -> Bool
        _cuandoRight (Suma  _ rightOp)  = _cuando CESuma  (constructor rightOp)
        _cuandoRight (Resta _ rightOp)  = _cuando CEResta (constructor rightOp)
        _cuandoRight (Mult  _ rightOp)  = _cuando CEMult  (constructor rightOp)
        _cuandoRight (Div   _ rightOp)  = _cuando CEDiv   (constructor rightOp)
        _cuandoRight _                  = False
        -- decision general de que combinaciones de parentesis se pueden omitor (tipo de expresion del padre e hijo)
        _cuando :: ConstructorExpr -> ConstructorExpr -> Bool
        _cuando _      CEConst  = False
        _cuando _      CERango  = False
        _cuando CESuma CESuma   = False
        _cuando CEMult CEMult   = False
        _cuando _      _        = True

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
