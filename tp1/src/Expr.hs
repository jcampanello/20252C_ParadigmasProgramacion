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
import Text.Read (Lexeme(String))

-- | Expresiones aritméticas con rangos
data Expr = Const Float
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
    IDEA:   se implementa el esquema de recursión primitiva para el cual se requieren n funciones (una por cada constructor
            disponible en el tipo Expr). Estas funciones toman:
            1:  los mismos atributos del constructor si no son constructores recursivos y retornan el resultado de evaluar
                los mismos
            2: los atributos del constructor y los resultados de evaluar recursivamente cada atributo del constructor
            Esto es porque la recursión primitiva permite "ver" el resto de la información en cada punto de procesamiento
-}
recrExpr :: (Float -> a) -> (Float -> Float -> a) -> (Expr -> Expr -> a -> a -> a) -> (Expr -> Expr -> a -> a -> a) -> (Expr -> Expr -> a -> a -> a) -> (Expr -> Expr -> a -> a -> a) -> Expr -> a
recrExpr fConst fRango fSuma fResta fMult fDiv expr =
    case expr of
        Const f             -> fConst f
        Rango s e           -> fRango s e
        Suma expr1 expr2    -> fSuma  expr1 expr2 (rec expr1) (rec expr2)
        Resta expr1 expr2   -> fResta expr1 expr2 (rec expr1) (rec expr2)
        Mult expr1 expr2    -> fMult  expr1 expr2 (rec expr1) (rec expr2)
        Div expr1 expr2     -> fDiv   expr1 expr2 (rec expr1) (rec expr2)
    where
        rec e = recrExpr fConst fRango fSuma fResta fMult fDiv e

-- | @recrExpr fConst fRango fSuma fResta fMult fDiv expr@ procesa @expr@ utilizando el esquema de Recursión Estructural,
-- donde @fConst@, @fRango@, @fSuma@, @fResta@, @fMult@ y @fDiv@ son las funciones específicas para procesar cada
-- constructor de Expr
{-
    IDEA:   se implementa el esquema de recursión estructural para el cual se requieren n funciones (una por cada constructor
            disponible en el tipo Expr). Estas funciones toman los atributos de dicho constructor
-}
foldExpr :: (Float -> a) -> (Float -> Float -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr fConst fRango fSuma fResta fMult fDiv expr =
    case expr of
        Const f             -> fConst f
        Rango s e           -> fRango s e
        Suma expr1 expr2    -> fSuma  (rec expr1) (rec expr2)
        Resta expr1 expr2   -> fResta (rec expr1) (rec expr2)
        Mult expr1 expr2    -> fMult  (rec expr1) (rec expr2)
        Div expr1 expr2     -> fDiv   (rec expr1) (rec expr2)
    where
        rec e = foldExpr fConst fRango fSuma fResta fMult fDiv e


-- | Evaluar expresiones dado un generador de números aleatorios
{-
    IDEA:     se procesa el árbol representado por la expresión, de forma que para cada nodo se arma una función
              (currificada) que resuelve la operación pero a la cual le queda el parámetro generador pendiente
              (evaluacion parcial). Hay 3 tipos de función generada, asociada a los tipos de nodos de la
              expresión:
                - const (retorna un número constante)
                - rango (retorna dameUno con el rango, pero que no queda evaluada por faltar el generador)
                - operador binario (para +, -, * y /) que toma dos operandos (left y right) y aplica, pasando el
                  generador por el primero operando y el generador actualizado al segundo
              NOTAR que en el caso Div, se pasa una función específica y no el operador (/). Esto es para lograr
              que la evaluación del caso Div sea total. Esta función observa el divisor y si es 0, entonces
              satura el resultado en +/- Infinito (según el signo del dividendo)
-}
eval :: Expr -> G Float
eval = foldExpr fConst fRango fSuma fResta fMult fDiv
    where
        -- las funciones directas en uso
        fConst f = _const f
        fRango s e = dameUno (s, e)
        fSuma l r = _binOper (+) l r
        fResta l r = _binOper (-) l r
        fMult l r = _binOper (*) l r
        fDiv l r = _binOper _operDiv l r
        -- auxiliares para Const y Div
        _const f gen = (f, gen)
        _operDiv l r = if r == 0 then if l < 0 then infinitoNegativo else infinitoPositivo else l / r
        -- procesamos operaciones binarias
        _binOper :: (Float -> Float -> Float) -> G Float -> G Float -> G Float
        _binOper op opLeft opDer gen = (op valorIzq valorDer, genDer)
            where
                (valorIzq, genIzq)  = opLeft gen
                (valorDer, genDer) = opDer genIzq




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
armarHistograma m n f g = (histograma m (rangoInicio, rangoFin) muestras, g_modificado)
    where
        (muestras, g_modificado) = muestra f n g
        (rangoInicio, rangoFin) = rango95 muestras



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
{-
    IDEA:   La resolución utiliza recrExpr debido a que es necesario poder evaluar el tipo de los hijos para los
            casos de constructores recursivos. Esta evaluación permite decidir en qué casos utilizar o no paréntesis.
            (por ej: si el nodo padre es una suma y su hijo izquierdo también, entonces no se pondrán paréntesis alrededor
            de la expresión que representa al dicho hijo).
            Esto implica la necesidad de utilizar recursión primitiva, de ahí el uso de recrExpr.
            Respecto de la resolución, se consideran 3 tipos de casos:
                - Const en donde simplemente se muestra el número de punto flotante
                - Range en donde se concatenan los dos valores de punto flotante junto con "~"
                - Operadores Binarios (Resta, Div, Suma, Mult). Estos se resuelven todos de la misma forma:
                    - Cada operando se resuelve independientemente, decidiendo si para el tipo de nodo padre y el tipo 
                      del nodo del hijo (left o right) es necesario utilizar paréntesis
                    - Se concatenan la resolución de ambos operadores junto con el string que corresponde al operando
            La función auxiliar armarOperando simplifica la implementación extrayendo código común y la decisión de
            cuando utilizar paréntesis queda delegada a la función usarParen, que decide en función del constructor
            del padre e hijo.
-}
mostrar :: Expr -> String
mostrar expr = recrExpr fConst fRango fSuma fResta fMult fDiv expr
    where
        -- casos simples - constante y rango
        fConst f                        = show f
        fRango s e                      = show s ++ "~" ++ show e
        -- casos complejos - operadores binarios
        fSuma   expr1 expr2 str1 str2   = armaOperando CESuma  expr1 str1 ++ " + " ++ armaOperando CESuma  expr2 str2
        fResta  expr1 expr2 str1 str2   = armaOperando CEResta expr1 str1 ++ " - " ++ armaOperando CEResta expr2 str2
        fMult   expr1 expr2 str1 str2   = armaOperando CEMult  expr1 str1 ++ " * " ++ armaOperando CEMult  expr2 str2
        fDiv    expr1 expr2 str1 str2   = armaOperando CEDiv   expr1 str1 ++ " / " ++ armaOperando CEDiv   expr2 str2

        -- armamos texto para operando
        armaOperando :: ConstructorExpr -> Expr -> String -> String
        armaOperando cons expr str = maybeParen (usarParen cons (constructor expr)) str

        -- decidimos si se necesitan los paréntesis
        usarParen :: ConstructorExpr -> ConstructorExpr -> Bool
        usarParen _        CEConst = False
        usarParen _        CERango = False
        usarParen CESuma   CESuma  = False
        usarParen CEMult   CEMult  = False
        usarParen _        _       = True


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
