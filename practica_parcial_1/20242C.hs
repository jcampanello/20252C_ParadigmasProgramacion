data Buffer a = Empty | Write Int a (Buffer a) | Read Int (Buffer a)
    deriving (Show)


buf = Write 1 'a' $ Write 2 'b' $ Write 1 'c' $ Empty

-- a)   Dar el tipo y definir la funcion foldBuffer y recBuffer, que implementan respectivamente los esquemas de
--      recursion estructural y primitiva para el tipo Buffer a. Solo en este inciso se permite usar recursion explıcita.

foldBuffer :: b -> (Int -> a -> b -> b) -> (Int -> b -> b) -> Buffer a -> b
foldBuffer cEmpty cWrite cRead arbol = case arbol of
                Empty                   -> cEmpty
                Write idx val buffer    -> cWrite idx val (rec buffer)
                Read idx buffer         -> cRead idx (rec buffer)
    where
        rec = foldBuffer cEmpty cWrite cRead



recBuffer :: b -> (Buffer a -> Int -> a -> b -> b) -> (Buffer a -> Int -> b -> b) -> Buffer a -> b
recBuffer cEmpty cWrite cRead arbol = case arbol of
                Empty                   -> cEmpty
                Write idx val buffer    -> cWrite buffer idx val (rec buffer)
                Read idx buffer         -> cRead buffer idx (rec buffer)
    where
        rec = recBuffer cEmpty cWrite cRead



-- b)   Definir la funcion posicionesOcupadas :: Buffer a -> [Int], que lista las posiciones ocupadas en un buffer (sin
--      posiciones repetidas).
--      Por ejemplo:
--
--          posicionesOcupadas buf ===> [1, 2]
--

posicionesOcupadas :: Buffer a -> [Int]
posicionesOcupadas = foldBuffer
                []                                                                  -- vacio
                (\idx val pos   -> if elem idx pos then pos else idx : pos)         -- write
                (\idx pos       -> if elem idx pos then removeIdx idx pos else pos) -- read
    where
        removeIdx idx pos = filter ((/=) idx) pos



-- c)   Definir la funcion contenido :: Int -> Buffer a -> Maybe a, que devuelva el contenido de una posicion en un buffer
--      si hay algo en ella, y Nothing en caso contrario.
--      Por ejemplo:
--
--          contenido 1 buf             ===> Just 'a'
--          contenido (-2) buf          ===> Nothing
--          contenido 1 (Read 1 buf)    ===> Nothing
--


contenido :: Int -> Buffer a -> Maybe a
contenido pos buffer = foldBuffer
                Nothing                                                             -- vacio
                (\idx val val'  -> if idx == pos then Just val else val')           -- write
                (\idx val'      -> if idx == pos then Nothing else val')            -- read
                buffer



-- d)   Definir la funcion puedeCompletarLecturas :: Buffer a -> Bool, que indique si todas las lecturas pueden completarse
--      exitosamente (es decir, si cada vez que se intenta leer una posicion, hay algo escrito en ella).
--      Por ejemplo:
--
--          puedeCompletarLecturas (Read 1 Empty)           ===> False
--          puedeCompletarLecturas (Read 1 buf)             ===> True
--          puedeCompletarLecturas (Read 1 $ Read 1 buf)    ===> False


puedeCompletarLecturas :: Buffer a -> Bool
puedeCompletarLecturas = recBuffer
                True                                                                -- vacio
                (\buffer idx val puede  -> True)                                    -- write
                (\buffer idx puede      -> case (contenido idx buffer) of
                                            Nothing             -> False
                                            otherwise           -> puede)           -- read




-- e)   Definir la funcion deshacer :: Buffer a -> Int -> Buffer a, que dados un buffer y un numero natural n (es decir, un Int
--      que por contexto de uso no es negativo), deshaga las ultimas n operaciones del buffer, sacando los n constructores mas
--      externos para obtener un buffer como el original antes de realizar dichas operaciones. Si se realizaron menos de n
--      operaciones, el resultado debe quedar vacıo.
--      Por ejemplo:
--
--          deshacer buf 2 ===> Write 1 'c' Empty
--
--      Pista: aprovechar la currificacion y utilizar evaluacion parcial.
--

deshacer :: Buffer a -> Int -> Buffer a
deshacer buffer = foldBuffer
                        (const Empty)
                        (\idx val buf n -> if n == 0 then Write idx val (buf 0) else buf (n - 1))
                        (\idx buf n     -> if n == 0 then Read idx (buf 0) else buf (n - 1))
                        buffer

