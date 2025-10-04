data AT a = NilT | Tri a (AT a) (AT a) (AT a)
    deriving (Show)


at1 = Tri 1 (Tri 2 NilT NilT NilT) (Tri 3 (Tri 4 NilT NilT NilT) NilT NilT) (Tri 5 NilT NilT NilT)


-- a)   Dar el tipo y definir la funcion foldAT que implementa el esquema de recursion estructural para el
--      tipo AT a. Solo en este inciso se permite usar recursion explıcita.

foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT cNil cTri arbol = case arbol of
                NilT            -> cNil
                Tri r i m d     -> cTri r (rec i) (rec m) (rec d)
    where
        rec = foldAT cNil cTri


-- b)   Definir la funcion preorder :: AT a -> [a], que lista los nodos de un arbol ternario en el orden en
--      que aparecen: primero la raız, despues los nodos del subarbol izquierdo, luego los del medio y
--      finalmente los del derecho.
--      Por ejemplo:
--
--          preorder at1 ===> [1, 2, 3, 4, 5]
--

preorder :: Foldable a => AT a -> [a]
preorder = foldAT [] (\r i m d -> r : (i ++ m ++ d))



-- c)   Definir la funcion mapAT :: (a -> b) -> AT a -> AT b, analoga a la funcion map para listas, pero
--      para arboles ternarios.
--      Por ejemplo:
--
--          mapAT (+1) at1 ===> Tri 2 (Tri 3 NilT NilT NilT) (Tri 4 (Tri 5 NilT NilT NilT) NilT NilT) (Tri 6 NilT NilT NilT)
--

mapAT :: (a -> b) -> AT a -> AT b
mapAT mapper = foldAT NilT (\r i m d -> Tri (mapper r) i m d)



-- d)   Definir la funcion nivel :: AT a -> Int -> [a], que devuelve la lista de nodos del nivel correspondiente del
--      arbol, siendo 0 el nivel de la raız.
--      Por ejemplo:
--
--          nivel at1 1 ===> [2, 3, 5]
--
--      Pista: aprovechar la currificacion y utilizar evaluacion parcial
--

nivel :: Foldable a => AT a -> Int -> [a]
nivel arbol = foldAT (const []) (\r i m d n -> if n == 0 then [r] else (i (n-1)) ++ (m (n-1)) ++ (d (n-1)) ) arbol

