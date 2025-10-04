data ABNV a = Hoja a | Uni a (ABNV a) | Bi (ABNV a) a (ABNV a)
    deriving (Show)

abnv = Bi (Uni 2 (Hoja 1)) 3 (Bi (Hoja 4) 5 (Uni 2 (Hoja 7)))

-- a)   Definir las funciones foldABNV y recABNV, que implementan respectivamente los esquemas de recursion estructural y
--      primitiva para el tipo ABNV a. Solo en este inciso se permite usar recursion explicita.
--

foldABNV :: (a -> b) -> (a -> b -> b) -> (b -> a -> b -> b) -> ABNV a -> b
foldABNV cHoja cUni cBi arbol = case arbol of
        Hoja h          -> cHoja h
        Uni r i         -> cUni r (rec i)
        Bi i r d        -> cBi (rec i) r (rec d)
    where
        rec = foldABNV cHoja cUni cBi


recABNV :: (a -> b) -> (a -> ABNV a -> b -> b) -> (ABNV a -> ABNV a -> b -> a -> b -> b) -> ABNV a -> b
recABNV cHoja cUni cBi arbol = case arbol of
        Hoja h          -> cHoja h
        Uni r i         -> cUni r i (rec i)
        Bi i r d        -> cBi i d (rec i) r (rec d)
    where
        rec = recABNV cHoja cUni cBi

-- b)   Definir la funcion elemABNV::Eq a => a -> ABNV a -> Bool, que indica si un elemento pertenece a un arbol.
--      Por ejemplo:
--
--          elemABNV 7 abnv ===> True
--

elemABNV e = foldABNV
                    ((==) e)                            -- es la hoja
                    (\r i -> r == e || i)               -- es la raiz o quizas esta en el subarbol
                    (\i r d -> r == e || i || d)        -- es la raiz o quizas esta en subarbol izq o der


-- c)   Definir la funcion reemplazarUno::Eq a => a -> a -> ABNV a -> ABNV a que, dados dos elementos x e y y un arbol,
--      devuelve un arbol como el original, pero reemplazando la primera aparicion de x por y (la primera desde la raız
--      yendo de izquierda a derecha, en el orden de preorder).
--      Por ejemplo:
--
--          reemplazarUno 2 5 abnv ===> Bi (Uni 5 (Hoja 1)) 3 (Bi (Hoja 4) 5 (Uni 2 (Hoja 7)))
--          reemplazarUno 2 5 (Hoja 1) ===> Hoja 1
--

reemplazarUno :: Eq a => a -> a -> ABNV a -> ABNV a
reemplazarUno ori dst arbol = recABNV
                                (\h -> if h == ori then Hoja dst else Hoja h)       -- hoja
                                (\r iOri i -> if r == ori
                                                    then (Uni dst iOri)
                                                    else (Uni r i) )                -- uni
                                (\iOri dOri i r d -> if r == ori
                                                        then (Bi iOri dst dOri)
                                                        else if elemABNV ori iOri
                                                                then (Bi i r dOri)
                                                                else (Bi iOri r d)) -- bi
                                arbol


-- d)   Definir la funcion nivel::ABNV a -> Int -> [a], que liste todos los elementos del nivel indicado, de izquierda a
--      derecha, siendo 0 el nivel de la raız. Si el nivel no existe (ya sea por ser negativo o por superar la altura del
--      arbol), devolver [].
--      Por ejemplo:
--
--          nivel abnv 1 ===> [2,5]
--          nivel abnv 2 ===> [1,4,2]
--          nivel abnv 4 ===> []
--
--      Pista: aprovechar la currificacion y utilizar evaluacion parcial
--

nivel :: Eq a => ABNV a -> Int -> [a]
nivel arbol = foldABNV
                (\h n -> if n == 0 then [h] else [])
                (\r i n -> if n == 0 then [r] else i (n - 1))
                (\i r d n -> if n == 0 then [r] else (i (n - 1)) ++ (d (n - 1)))
                arbol

