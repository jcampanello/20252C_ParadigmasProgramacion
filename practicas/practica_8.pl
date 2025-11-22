

% ------------------------------------------------
% ------------------------------------------------
% ------------------------------------------------
%
% MOTOR DE BUSQUEDA DE PROLOG
%




% ------------------------------------------------
%
% EJERCICIO 1
%

padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).

% adicionales
padre(ramiro, analia).
padre(pablo, emma).
padre(pablo, mogh).
padre(mogh, worf).
padre(worf, alexander).

abuelo(X,Y) :- padre(X,Z), padre(Z,Y).



% ---------------------------
%
% i. ¿Cual el resultado de la consulta abuelo(X, manuel)?
%
% el resultado es:
%
% X = juan ;
% false
%


% ---------------------------
%
% ii. A partir del predicado binario padre, definir en Prolog los
% predicados binarios: hijo, hermano y descendiente.

hijo(Hijo, Padre) :- padre(Padre, Hijo).

hermano(Hermano1, Hermano2) :- padre(Padre, Hermano1), padre(Padre, Hermano2), Hermano1 \= Hermano2.

descendiente(Pers1, Pers2) :- padre(Pers1, Pers2).
descendiente(Pers1, Pers2) :- padre(Pers3, Pers2), descendiente(Pers1, Pers3).

% ---------------------------
%
% iii. Dibujar el arbol de busqueda de Prolog para la consulta descendiente(Alguien, juan).
%   
%   ?-  descendiente(Alguien, juan).
%       |
%       +-  padre(X, juan)
%       |   |
%       |   +-- false                                           FALLA
%       |
%       +-  padre(Pers3, juan), descendiente(Pers1, Pers3)      { Alguien := Pers3, Pers2 := juan }
%           |
%           +-- false                                           FALLA
%

% ---------------------------
%
% iv. ¿Qué consulta habría que hacer para encontrar a los nietos de juan?
%
% abuelo(juan, Nieto).
%

% ---------------------------
%
% v. ¿Cómo se puede definir una consulta para conocer a todos los hermanos de pablo?
%
%
% hermano(pablo, Hermano).
%


% ---------------------------
%
% vi. Considerar el agregado del siguiente hecho y regla:
%
%   ancestro(X, X).
%   ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).
% 
% y la base de conocimiento del ítem anterior.
%

ancestro(X, X).
ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).

% ---------------------------
%
% vii. Explicar la respuesta a la consulta ancestro(juan, X). ¿Qué sucede si se pide más de un resultado?
%
% Se obtienen todos los resultados esperables, pero luego se cuelga. El problema es que en un punto,
% ancestro(Z, Y) commienza a generar instanciaciones y siempre entran por ancestro(X, X) dando valores
% intermedios válidos esperando instanciación posterior.
%

% ---------------------------
%
% viii. Sugerir una solución al problema hallado en los puntos anteriores reescribiendo el programa de ancestr
%

ancestro_fixed(X, X).
ancestro_fixed(X, Y) :- padre(X, Z), ancestro_fixed(Z, Y).





% ------------------------------------------------
%
% EJERCICIO 2
%
% Sea el siguiente programa lógico:

vecino(X, Y, [_ | Ls]) :- vecino(X, Y, Ls).
vecino(X, Y, [X | [Y | _]]).

% ---------------------------
%
% i. Mostrar el árbol de búsqueda en Prolog para resolver vecino(5, Y, [5,6,5,3]), devolviendo todos los
% valores de Y que hacen que la meta se deduzca lógicamente del programa.
%
%   ?-  vecino(5, Y, [5, 6, 5, 3])
%       |
%       +-- vecino(5, 6, [5 | [6 | _]])             { X := 5, Y := 6 }
%       |   |
%       |   +-- true                                EXITO Y = 6
%       |
%       +-- vecino(5, Y, [6, 5, 3] )                { X := 5, Ls := [6, 5, 3] }
%           |
%           +-- vecino(5, Y, [5, 3])                { X := 5, Ls := [5, 3] }
%               |
%               +-- vecino(5, 3, [5 | [3 | _]])     { X := 5, Y := 3 }
%               |   |
%               |   +-- true                        EXITO Y = 3
%               |
%               +-- vecino(5, Y, [3])               { X := 5, Ls := [3] }
%                   |
%                   +-- false
%

% ---------------------------
%
% ii. Si se invierte el orden de las reglas, ¿los resultados son los mismos? ¿Y el orden de los resultados?
%
% los resultados son los mismos, pero en orden inverso. Porque primero reduce la lista y luego intenta
% entregar el vecino cuando se cumple la condicion.
%






% ------------------------------------------------
%
% EJERCICIO 3
%
%
% Considerar las siguientes definiciones:
%

natural(0).
natural(suc(X)) :- natural(X).

menorOIgual(X, X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
%
% ERROR si se pone aquí la regla:
%       menorOIgual(X, X) :- natural(X).
%


% ---------------------------
%
% i. Explicar qué sucede al realizar la consulta menorOIgual(0, X).
%
% El programa se cuelga porque comienza a generar sucesores para tratar de matchear. El problema
% es que la regla que corta la recursión debería estar primero, para que prolog la pueda encontrar.
%

% ---------------------------
%
% ii. Describir las circunstancias en las que puede colgarse un programa en Prolog. Es
% decir, ejecutarse infinitamente sin arrojar soluciones.
%
% Un programa se puede colgar cuando el árbol de búsqueda se torna infinito. Esto puede ocurrir
% por errores en la propia lógica, pero también porque el orden de las reglas no es el correcto.
%

% ---------------------------
%
% iii. Corregir la definición de menorOIgual para que funcione adecuadamente.
%
% La corrección implica cambiar el orden de las reglas y poner primero la regla que corta
% la recursividad.
%
% Una vez corregido, la consulta genera todos los valores que cumplen que 0 es menor o igual al valor
%




% ------------------------------------------------
% ------------------------------------------------
% ------------------------------------------------
%
% OPERACIONES SOBRE LISTAS
%




% ------------------------------------------------
%
% EJERCICIO 4
%
% Definir el predicado juntar(?Lista1,?Lista2,?Lista3), que tiene éxito si Lista3 es la concatenación
% de Lista1 y Lista2. Por ejemplo:
%   
%   ?- juntar([a, b, c], [d, e], [a, b, c, d, e]).      → true.
%   ?- juntar([a, b, c], [d, e], L).                    → L = [a, b, c, d, e].
%   ?- juntar([a, b, c], L, [a, b, c, d, e]).           → L = [d, e].
%   ?- juntar(L, [d, e], [a, b, c, d, e]).              → L = [a, b, c].
%   ?- juntar(L1, L2, [1, 2, 3]).                       → L1 = [], L2 = [1, 2, 3];
%                                                         L1 = [1], L2 = [2, 3];
%                                                         L1 = [1, 2], L2 = [3];
%                                                         L1 = [1, 2, 3], L2 = [].
%
% Al igual que la mayoría de los predicados, puede dar false después de agotar los resultados.
%
% Nota: este predicado ya está definido en prolog con el nombre append.
%

juntar_p8([], L2, L2).
juntar_p8([H|T], L2, [H|L]) :- juntar_p8(T, L2, L).





% ------------------------------------------------
%
% EJERCICIO 5
%
% Definir los siguientes predicados sobre listas usando append:

% ---------------------------
%
% i. last(?L, ?U), donde U es el último elemento de la lista L.

%! last_p8(?L, ?U)
last_p8(L, U) :- append(_, [U], L).


% ---------------------------
%
% ii. reverse(+L, ?R), donde R contiene los mismos elementos que L, pero en orden inverso.
% Ejemplo: reverse([a,b,c], [c,b,a]).
%
% Mostrar el árbol de búsqueda para el ejemplo dado.

%! reverse_p8(+L, ?R)
% REV1
reverse_p8([], []).
% REV2
reverse_p8(L, R) :- append([H], T, L), reverse_p8(T, TR), append(TR, [H], R).

/*

% APP1
append([], L, L).
% APP2
append([H|T], O, [H|R]) :- append(T, O, R).

?-  reverse_p8([a,b,c], [c,b,a]).
    |
    +-- % regla REV2                                                        reverse_p8(L, R) => append([H], T, L), reverse_p8(T, TR), append(TR, [H], R)
        { L := [a,b,c], R := [c,b,a] }
        append([H], T, [a,b,c]), reverse_p8(T, TR), append(TR, [H], [c,b,a]).
        |
        +-- regla APP2                                                      append([H], T, [a,b,c]) = append([Ha|Ta], Oa, [Ha|Ra]) => append(Ta, Oa, Ra)
            { Happ := H, H := a, Tapp := [], Oapp := T, Rapp := [b,c] },
            append([], T, [b,c]), reverse_p8(T, TR), append(TR, [a], [c,b,a]).
            |
            +-- regla APP1                                                  append([], L, L) => ""
                { T := [b,c] }
                reverse_p8([b,c], TR), append(TR, [a], [c,b,a]).
                |
                +-- regla REV2                                              reverse_p8(L, R) => append([H], T, L), reverse_p8(T, TR1), append(TR1, [H], R)
                    { L := [b,c], R := TR }
                    append([H], T, [b,c]), reverse_p8(T, TR1), append(TR1, [H], TR), append(TR, [a], [c,b,a]).
                    |
                    +-- regla APP2                                          append([H|T], O, [H|R]) => append(T, O, R)
                        { H := b, O := T, R := [c], T := [] }
                        append([], T, [c]), reverse_p8(T, TR1), append(TR1, [b], TR), append(TR, [a], [c,b,a]).
                        |
                        +-- regla APP1                                      append([], L, L) => ""
                            { T := [c] }
                            reverse_p8([c], TR1), append(TR1, [b], TR), append(TR, [a], [c,b,a]).
                            |
                            +-- regla REV2                                  reverse_p8(L, R) => append([H], T, L), reverse_p8(T, TR), append(TR, [H], R)
                                { L := [c], R := TR2 }
                                append([H], T, [c]), reverse_p8(T, TR2), append(TR2, [H], TR1), append(TR1, [b], TR), append(TR, [a], [c,b,a]).
                                |
                                +-- regla APP2                              append([H|T], O, [H|R]) => append(T, O, R)
                                    { H := c, O := T, R := [], T := [] }
                                    append([], T, []), reverse_p8(T, TR2), append(TR2, [c], TR1), append(TR1, [b], TR), append(TR, [a], [c,b,a]).
                                    |
                                    +-- regla APP1                          append([], L, L) => ""
                                        { T := [] }
                                        reverse_p8([], TR2), append(TR2, [c], TR1), append(TR1, [b], TR), append(TR, [a], [c,b,a]).
                                        |
                                        +-- regla REV1                      reverse_p8([], []) => ""
                                            { TR3 := [] }
                                            append([], [c], TR1), append(TR1, [b], TR), append(TR, [a], [c,b,a]).
                                            |
                                            +-- regla APP1                  append([], L, L) => ""
                                                { TR1 := [c] }
                                                append([c], [b], TR), append(TR, [a], [c,b,a]).
                                                |
                                                +-- regla APP2              append([H|T], O, [H|R]) => append(T, O, R)
                                                    { H := c, T := [], O := [b], TR := [c|R] }
                                                    append([], [b], R), append([c|R], [a], [c,b,a]).
                                                    |
                                                    +-- regla APP1          append([], L, L) => ""
                                                        { R := [b] }
                                                        append([c|[b]], [a], [c,b,a]) = append([c,b], [a], [c,b,a]).
                                                        |
                                                        +-- regla APP2      append([H|T], O, [H|R]) => append(T, O, R)
                                                            { H := c, T := [b], O := [a], R := [b,a] }
                                                            append([b], [a], [b,a]).
                                                            |
                                                            +-- regla APP2  append([H|T], O, [H|R]) => append(T, O, R)
                                                                { H := b, T := [], O := [a], R := [a] }
                                                                append([], [a], [a]).
                                                                |
                                                                +-- regla APP1 append([], L, L) => ""
                                                                    { L := [a] }
                                                                    SUCCESS

*/

% ---------------------------
%
% iii. prefijo(?P, +L), donde P es prefijo de la lista L.

%! prefijo_p8(?P, +L)
prefijo_p8(P, L) :- append(P, _, L).


% ---------------------------
%
% iv. sufijo(?S, +L), donde S es sufijo de la lista L.

%! sufijo_p8(?S, +L)
sufijo_p8(S, L) :- append(_, S, L).


% ---------------------------
%
% v. sublista(?S, +L), donde S es sublista de L.

%! sublista(?S, +L)
sublista_p8([], _).
sublista_p8(S, L) :- append(_, S1, L), append(S, _, S1), S \= [].


% ---------------------------
%
% vi. pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L.
% (Este predicado ya viene definido en Prolog y se llama member).
%

%! pertenece_p8(?X, +L)
pertenece_p8(X, L) :- append(_, [X|_], L).



% ------------------------------------------------
%
% EJERCICIO 6
%
% Definir el predicado aplanar(+Xs, -Ys), que es verdadero sii Ys contiene los elementos
% de todos los niveles de Xs, en el mismo orden de aparición. Los elementos de Xs son
% enteros, átomos o nuevamente listas, de modo que Xs puede tener una profundidad
% arbitraria. Por el contrario, Ys es una lista de un solo nivel de profundidad.
%
% Ejemplos:
%
%   aplanar([a, [3, b, []], [2]], L).           → L = [a, 3, b, 2]
%   aplanar([[1, [2, 3], [a]], [[[]]]], L).     → L = [1, 2, 3, a]
%
% Nota: este predicado ya está definido en prolog con el nombre flatten.
%

%! aplanar(+Xs, -Ys)
aplanar([], []).
aplanar([H | T], Plana) :- valores_planos(H, Hs), aplanar(T, Ts), append(Hs, Ts, Plana).

valores_planos(H, [H]) :- atom(H).
valores_planos(H, [H]) :- number(H).
valores_planos(H, Plana) :- is_list(H), aplanar(H, Plana).




% ------------------------------------------------
%
% EJERCICIO 7
%
% Definir los siguientes predicados, usando member y / o append según sea conveniente:



% ---------------------------
%
% i. intersección(+L1, +L2, -L3), tal que L3 es la intersección sin repeticiones de las
% listas L1 y L2, respetando en L3 el orden en que aparecen los elementos en L1.
%
% partir(N, L, L1, L2), donde L1 tiene los N primeros elementos de L, y L2 el resto. Si
% L tiene menos de N elementos el predicado debe fallar. ¿Cuán reversible es este
% predicado? Es decir, ¿qué parámetros pueden estar indefinidos al momento de la invocación?

%! intersección(+L1, +L2, -L3)
intersección(L1, L2, L3Filtrada) :- intersección_(L1, L2, L3), sacarDuplicados(L3, L3Filtrada).

intersección_([], _, []).
intersección_([H|T], L2, L3) :- member(H, L2), intersección_(T, L2, L3Bis), append([H], L3Bis, L3).
intersección_([H|T], L2, L3) :- not(member(H, L2)), intersección_(T, L2, L3).

%! partir(+N, +L, ?L1, ?L2)
partir(N, L, L1, L2) :- append(L1, L2, L), length(L1, LL1), N is LL1.



% ---------------------------
%
% ii. borrar(+ListaOriginal, +X, -ListaSinXs), que elimina todas las ocurrencias de X de
% la lista ListaOriginal.
%

%! borrar(+ListaOriginal, +X, -ListaSinXs)
borrar([], _, []).
borrar([H|T], X, LFinal) :- H = X, borrar(T, X, LFinal).
borrar([H|T], X, [H|LFinal]) :- H \= X, borrar(T, X, LFinal).

% ---------------------------
%
%
% iii. sacarDuplicados(+L1, -L2), que saca todos los elementos duplicados de la lista L1.

%! sacarDuplicados(+L1, -L2)
sacarDuplicados([], []).
sacarDuplicados([H|T], Resultado) :- sacarDuplicados(T, R1), member(H, R1), borrar(R1, H, R2), append([H], R2, Resultado).
sacarDuplicados([H|T], Resultado) :- sacarDuplicados(T, R1), not(member(H, R1)), append([H], R1, Resultado).


% ---------------------------
%
%
% iv. permutación(+L1, ?L2), que tiene éxito cuando L2 es permutación de L1. ¿Hay una
% manera más eficiente de definir este predicado para cuando L2 está instanciada?

%! permutación(+L1, ?L2)
permutación([], _).
permutación([H|T], L2) :- member(H,L2), permutación(T, L2).

permutaciónBis(L1, L2) :- intersección(L1, L2, _).


% ---------------------------
%
% v. reparto(+L, +N, -LListas) que tenga éxito si LListas es una lista de N listas
% (N ≥ 1) de cualquier longitud - incluso vacías - tales que al concatenarlas se obtiene
% la lista L.

%! reparto(+L, +N, -LListas)
reparto([], 0, []).
reparto(L, N, LListas) :-
    N > 0,
    append(Parte, Resto, L),
    N1 is N-1,
    reparto(Resto, N1, LListasRec),
    append([Parte], LListasRec, LListas).

% ---------------------------
%
% vi. repartoSinVacías(+L, -LListas) similar al anterior, pero ninguna de las listas de
% LListas puede ser vacía, y la longitud de LListas puede variar.


%! repartoSinVacías(+L, -LListas)
repartoSinVacías([], []).
repartoSinVacías(L, LListas) :-
    append(Parte, Resto, L),
    Parte \= [],
    repartoSinVacías(Resto, LListasRec),
    append([Parte], LListasRec, LListas).





% ------------------------------------------------
%
% EJERCICIO 8
%
% Definir el predicado parteQueSuma(+L,+S,-P) que es verdadero cuando P es una lista con elementos de L que suman S. Por ejemplo:
%
%   ?- parteQueSuma([1,2,3,4,5], 9, P).
%   P = [1, 3, 5] ;
%   P = [2, 3, 4] ;
%   P = [4, 5] ;
%   false.
%

%! parteQueSuma(+L, +S, -P)
parteQueSuma(_, 0, []).
parteQueSuma(L, S, P) :- partes(L, C), sum_list(C, Sum), S is Sum, P = C.

%! partes(+L, -C)
partes([], []).
partes([H|T], C) :- partes(T, CTail), append([H], CTail, C).
partes([_|T], C) :- partes(T, C).



% ------------------------------------------------
% ------------------------------------------------
% ------------------------------------------------
%
% INSTANCIACIÓN Y REVERSIBILIDAD
%

% ------------------------------------------------
%
% EJERCICIO 9
%
% Considerar el siguiente predicado:
%
%   desde(X, X).
%   desde(X, Y) :- N is X + 1, desde(N, Y).
%

desde(X, X).
desde(X, Y) :- N is X + 1, desde(N, Y).

%
% Esto genera los números enteros desde X hacia +Infinito
%

% ---------------------------
%
% i. ¿Cómo deben instanciarse los parámetros para que el predicado funcione? (Es decir, para
% que no se cuelgue ni produzca un error). ¿Por qué?
%

% Se debe instanciar el parámetro X y dejar Y libre.
% Si se instancia un valor de Y que es mayor que X entonces devolverá un true y luego se va a colgar,
% porque N no tiene un corte por N == Y.
% Si se instancia un valor de Y que es manor que X, entonces se colgará (porque N crece, y nunca va
% a poder llegar al valor de Y).

% ---------------------------
%
% ii. Dar una nueva versión del predicado que funcione con la instanciación desdeReversible(+X, ?Y),
% tal que si Y está instanciada, sea verdadero si Y es mayor o igual que X, y si no lo está genere
% todos los Y de X en adelante.
%

%! desdeReversible(+X, ?Y)
desdeReversible(X, Y) :- nonvar(Y), Y >= X.
desdeReversible(X, Y) :- var(Y), desde(X, Y).



% ------------------------------------------------
%
% EJERCICIO 10
%
% Definir el predicado intercalar(L1, L2, L3), donde L3 es el resultado de intercalar uno a uno los
% elementos de las listas L1 y L2. Si una lista tiene longitud menor, entonces el resto de la lista
% más larga es pasado sin cambiar. Indicar la reversibilidad, es decir si es posible obtener L3 a
% partir de L1 y L2, y viceversa.
%
% Ejemplo: intercalar([a,b,c], [d,e], [a,d,b,e,c]).
%

%! intercalar(?L1, ?L2, ?L3)
intercalar([], L2, L2).
intercalar(L1, [], L1) :- length(L1, L1Len), L1Len > 0.         % excluyente con el caso previo
intercalar([H1|T1], [H2|T2], [H1,H2|L3]) :- intercalar(T1, T2, L3).

% Si es reversible. En particular, hay 4 casos diferentes:
%
% - L1 y L2 definidos, L3 no:   aquí el predicado calcula L3 (es el caso que cumple el requerimiento)
% - L1 y L3 definidos, L2 no:   en este caso, los elementos de L3 deben ir correspondiendo al head de
%                               L1 y L2. En la medida que haya coincidencia, el predicado recursivo
%                               aplica, puede separar y por lo tanto saber cuando aplica el resto
% - L2 y L3 definidos, L1 no:   este caso es similar al anterior
% - L3 definido, L1 y L2 no:    el predicado va a generar todas las posibles opciones de L1 y L2 que
%                               forman L3. Mirará primero los casos vacíos y luego comenzará a
%                               separar los dos primeros elementos (uno a cada lista) y luego generando
%                               los casos de forma recursiva (tomando los dos triviales, que acumulan
%                               todo el resto en una de las listas y luego tomando otros dos elementos).
%



% ------------------------------------------------
%
% EJERCICIO 11
%
% Un árbol binario se representará en Prolog con:
%
%   - nil, si es vacío.
%   - bin(izq, v, der), donde v es el valor del nodo, izq es el subárbol izquierdo y der es el subárbol derecho.
%
% Definir predicados en Prolog para las siguientes operaciones: vacío, raiz, altura y cantidadDeNodos. Asumir
% siempre que el árbol está instanciado.
%

%! bin(?I, ?R, ?D)
bin(_, _, _).

%! arbolBacío(?A)
arbolVacío(nil).

%! arbolRaiz(?A, ?R)
arbolRaiz(bin(_, R, _), R).

%! arbolAltura(+A, ?H)
arbolAltura(nil, 0).
arbolAltura(bin(I, _, D), H) :- arbolAltura(I, HI), arbolAltura(D, HD), H is max(HI, HD) + 1.

%! arbolCantidadDeNodos(A, Q)
arbolCantidadDeNodos(nil, 0).
arbolCantidadDeNodos(bin(I, _, D), Q) :- arbolCantidadDeNodos(I, QI), arbolCantidadDeNodos(D, QD), Q is QI + QD + 1.



% ------------------------------------------------
%
% EJERCICIO 12
%
% Definir los siguientes predicados, utilizando la representación de árbol binario definida en el ejercicio 11:


% ---------------------------
%
% i. inorder(+AB, -Lista), que tenga éxito si AB es un árbol binario y Lista la lista de sus nodos según el
% recorrido inorder.

%! arbolInOrder(+AB, -Lista)
arbolInOrder(nil, []).
arbolInOrder(bin(I, R, D), Lista) :- arbolInOrder(I, ListaI), arbolInOrder(D, ListaD), append(ListaI, [R|ListaD], Lista).


% ---------------------------
%
% ii. arbolConInorder(+Lista,-AB), versión inversa del predicado anterior.

%! arbolConInorder(+Lista, -AB)
arbolConInOrder([], nil).
arbolConInOrder(Lista, bin(ABI, R, ABD)) :-
    % separamos posibles casos de lo que va a izq, der y la raiz
    append(ListaI, ListaResto, Lista), append([R], ListaD, ListaResto),
    % generamos posibles casos de izq y der
    arbolConInOrder(ListaI, ABI), arbolConInOrder(ListaD, ABD).


% ---------------------------
%
% iii. aBB(+T), que será verdadero si T es un árbol binario de búsqueda.

%! ABB(+A)
aBB(nil).
aBB(bin(I, R, D)) :-
    arbolInOrder(I, ListaI), append(ListaI, [R], ListaIR), listaAscendente(ListaIR),
    arbolInOrder(D, ListaD), listaAscendente([R|ListaD]).

%! listaAscendente(+L)
listaAscendente([]).
listaAscendente([_]).
listaAscendente([H1,H2|T]) :- H1 =< H2, listaAscendente([H2|T]).


% ---------------------------
%
% iv. aBBInsertar(+X, +T1, -T2), donde T2 resulta de insertar X en orden en el árbol T1.
% Este predicado ¿es reversible en alguno de sus parámetros? Justificar.

%! aBBInsertar(+X, +T1, -T2)
aBBInsertar(X, nil, bin(nil, X, nil)).
aBBInsertar(X, bin(I, R, D), T2) :- X < R, aBBInsertar(X, I, I2), T2 = bin(I2, R, D).
aBBInsertar(X, bin(I, R, D), T2) :- R =< X, aBBInsertar(X, D, D2), T2 = bin(I, R, D2).




% ------------------------------------------------
% ------------------------------------------------
% ------------------------------------------------
%
% GENERATE & TEST
%

% ------------------------------------------------
%
% EJERCICIO 13
%
% Definir el predicado coprimos(-X,-Y), que genere uno a uno todos los pares de números
% naturales coprimos (es decir, cuyo máximo común divisor es 1), sin repetir resultados.
% Usar la función gcd del motor aritmético.
%

%! coprimos(-X, -Y)
coprimos(X, Y) :- nonvar(X), nonvar(Y), 1 is gcd(X, Y).
coprimos(X, Y) :- nonvar(X), var(Y), coprimos(Y, X).
coprimos(X, Y) :- var(X), desde(1, Cota), between(1, Cota, X), Y is Cota - X, Y > 0, 1 is gcd(X, Y).


% ------------------------------------------------
%
% EJERCICIO 14
%
% Un cuadrado semi-mágico es una matriz cuadrada de naturales (incluido el cero) donde todas las filas
% de la matriz suman lo mismo. Por ejemplo:
%
%       1 3 0
%       2 2 0 todas las filas suman 4
%       1 1 2
%
% Representamos la matriz como una lista de filas, donde cada fila es una lista de naturales. El ejemplo
% anterior se representaría de la siguiente manera:
%
%       [ [1,3,0], [2,2,0], [1,1,2] ]
%


% ---------------------------
%
% i. Definir el predicado cuadradoSemiMagico(+N, -XS). El predicado debe ir devolviendo matrices (utilizando
% la representación antes mencionada), que sean cuadrados semi-mágicos de dimensión N * N. Dichas matrices
% deben devolverse de manera ordenada: primero aquellas cuyas filas suman 0, luego 1, luego 2, etc. No es
% necesario utilizar la técnica Generate & Test.
%
% Ejemplo: cuadradoSemiMagico(2, X). devuelve:
%
% X = [[0, 0], [0, 0]] ;
% X = [[0, 1], [1, 0]] ; X = [[1, 0], [1, 0]] ; X = [[0, 1], [0, 1]] ; X = [[1, 0], [0, 1]] ;
% X = [[0, 2], [0, 2]] ; etc
%

%! cuadradoSemiMagico(+N, -XS)
cuadradoSemiMagico(N, XS) :- desde(0, Suma), filasSemiMagicas(N, N, Suma, XS).

%! filasSemiMagicas(+N, +Suma, -Filas).
filasSemiMagicas(_, 0, _, []).
filasSemiMagicas(N, Actual, Suma, Filas) :-
    Actual > 0,
    filaSemiMagica(N, Suma, Fila),
    ActualResto is Actual - 1,
    filasSemiMagicas(N, ActualResto, Suma, FilasResto),
    append([Fila], FilasResto, Filas).

%! filaSemiMagica(+N, +Suma, -Fila)
filaSemiMagica(0, _, []).
filaSemiMagica(N, Suma, Fila) :-
    N > 0,
    between(0, Suma, Valor),
    SumaResto is Suma - Valor,
    NResto is N - 1,
    filaSemiMagica(NResto, SumaResto, FilaResto),
    append([Valor], FilaResto, Fila),
    sum_list(Fila, Total),
    Total is Suma.


% ---------------------------
%
% ii. Definir utilizando Generate & Test el predicado cuadradoMagico(+N, -XS), que instancia XS con cuadrados cuyas
% filas y columnas suman todas un mismo valor.
%

%! matriz(+F, +C, -M)
matriz(F, C, M) :-
    length(M, F),
    nth1(1, M, FirstRow),
    length(FirstRow, C),
    maplist(same_length(FirstRow), M).

%! transponer(+M, -MT)
transponer(M, MT) :-
    matriz(_, C, M),                                        % identificamos las cant columnas de M
    matriz(C, 0, MT0),                                      % la transpuesta tiene C filas (inicalmente vacías)
    foldl(agregar_a_columna, M, MT0, MT).                   % plegamos la matriz inicial, esto genera la transpuesta

%! agregar_a_columna(+FilaOriginal, +TranspuestaVacia, -MatrizTranspuesta)
agregar_a_columna([], [], []).
agregar_a_columna([HR | TR], [HM0 | TM0], [HMT | TMT]) :-
    agregar_a_columna(TR, TM0, TMT),
    append(HM0, [HR], HMT).



%! cuadradoMagico(+N, -XS)
cuadradoMagico(N, CSemiMag) :-
    % generate
    cuadradoSemiMagico(N, CSemiMag),
    % test
    columnasMagicas(CSemiMag).

%! columnasMagicas(+CSemiMag)
columnasMagicas(CSemiMag) :-
    nth1(1, CSemiMag, FirstRow),
    transponer(CSemiMag, CSemiMagTrans),
    append([FirstRow], CSemiMagTrans, FilaYColumnas),
    obtenerSumas(FilaYColumnas, Sumas),
    list_to_set(Sumas, Set),
    Set = [_].

%! obtenerSumas(+Rows, -Sumas)
obtenerSumas([], []).
obtenerSumas([Row|Rest], Sumas) :- sum_list(Row, Suma), obtenerSumas(Rest, SumasRest), append([Suma], SumasRest, Sumas).



% ------------------------------------------------
%
% EJERCICIO 15
%
% En este ejercicio trabajaremos con triángulos. La expresión tri(A,B,C) denotará el triángulo cuyos lados tienen
% longitudes A, B y C respectivamente. Se asume que las longitudes de los lados son siempre números naturales.
%
% Implementar los siguientes predicados:


% ---------------------------
%
% i. esTriángulo(+T) que, dada una estructura de la forma tri(A,B,C), indique si es un triángulo válido. En un
% triángulo válido, cada lado es menor que la suma de los otros dos, y mayor que su diferencia (y obviamente mayor
% que 0).

%! esTriangulo(+T)
esTriangulo(tri(A, B, C)) :-
    A > 0, B > 0, C > 0,
    A < B + C, A > abs(B - C),
    B < A + C, B > abs(A - C),
    C < A + B, C > abs(A - B).


% ---------------------------
%
% ii. perímetro(?T, ?P), que es verdadero cuando T es un triángulo (válido) y P es su perímetro. No se deben generar
% resultados repetidos (no tendremos en cuenta la congruencia entre triángulos: si dos triángulos tienen las mismas
% longitudes, pero en diferente orden, se considerarán diferentes entre sí). El predicado debe funcionar para cualquier
% instanciación de T y P (ambas instanciadas, ambas sin instanciar, una instanciada y una no; no es necesario que
% funcione para triángulos parcialmente instanciados), debe generar todos los resultados válidos (sean finitos o
% infinitos), y no debe colgarse (es decir, no debe seguir ejecutando infinitamente sin producir nuevos resultados).
% Por ejemplo:
%
%   ?- perímetro(tri(3, 4, 5), 12).         → true.
%   ?- perímetro(T, 5).                     → T = tri(1, 2, 2) ; T = tri(2, 1, 2) ; T = tri(2, 2, 1) ; false.
%   ?- perímetro(tri(2, 2, 2), P).          → P = 6.
%   ?- perímetro(T, P).                     → T = tri(1, 1, 1), P = 3 ; T = tri(1, 2, 2), P = 5 ; ...
%

%! perímetro(?T, ?P)
perímetro(T, P) :- nonvar(T), esTriangulo(T), T = tri(A,B,C), P is A + B + C.
perímetro(T, P) :- var(T), desde(3, P), generarTernas(P, A, B, C), esTriangulo(tri(A, B, C)), T = tri(A, B, C).

%! generarTernas(+P, -A, -B, -C)
generarTernas(P, A, B, C) :- between(0, P, A), Resto is P - A, between(0, Resto, B), C is P - A - B.

% ---------------------------
%
% iii. triángulo(-T), que genera todos los triángulos válidos, sin repetir resultados.

%! triángulo(-T)
triángulo(T) :- perímetro(T, _).






% ------------------------------------------------
% ------------------------------------------------
% ------------------------------------------------
%
% NEGACIÓN POR FALLA Y CUT
%


% ------------------------------------------------
%
% EJERCICIO 16
%
% A Ana le gustan los helados que sean a la vez cremosos y frutales. En una heladería de su barrio, se encontró
% con los siguientes sabores:
%

frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(dulceDeLeche).
cremoso(americana).
cremoso(frutilla).

%
% Ana desea comprar un cucurucho con sabores que le gustan. El cucurucho admite hasta 2 sabores. Los siguientes
% predicados definen las posibles maneras de armar el cucurucho.
%

leGusta(X) :- frutal(X), cremoso(X).
cucurucho(X, Y) :- leGusta(X), leGusta(Y).


% ---------------------------
%
% i. Escribir el árbol de búsqueda para la consulta
%
%   ?- cucurucho(X, Y).
%

/*

ARBOL DE BUSQUEDA

?-  cucurucho(X, Y).
  +-- leGusta(X), leGusta(Y).
    +-- frutal(X), cremoso(X), leGusta(Y).
      +-- frutal(frutilla), cremoso(frutilla), leGusta(Y).
        +-- frutal(frutilla), cremoso(frutilla), frutal(Y), cremoso(Y).
          +-- frutal(frutilla), cremoso(frutilla), frutal(frutilla), cremoso(frutilla).   PASS { X := frutilla, Y := frutilla }
          +-- frutal(frutilla), cremoso(frutilla), frutal(banana), cremoso(banana).       PASS { X := frutilla, Y := banana }
          +-- frutal(frutilla), cremoso(frutilla), frutal(manzana), cremoso(manzana).     FAIL
      +-- frutal(banana), cremoso(banana), leGusta(Y).
        +-- frutal(banana), cremoso(banana), frutal(Y), cremoso(Y).
          +-- frutal(banana), cremoso(banana), frutal(frutilla), cremoso(frutilla).       PASS { X := banana, Y := frutilla }
          +-- frutal(banana), cremoso(banana), frutal(banana), cremoso(banana).           PASS { X := banana, Y := banana }
          +-- frutal(banana), cremoso(banana), frutal(manzana), cremoso(manzana).         FAIL
      +-- frutal(manzana), cremoso(manzana), leGusta(Y).                                  FAIL

*/


% ---------------------------
%
% ii. Indicar qué partes del árbol se podan al colocar un ! en cada ubicación posible en las definiciones de
% cucurucho y leGusta.
%
% Con el CUT en leGusta, va a buscar el primer sabor frutal, y luego verificar que sea cremoso. Y no buscará más combinaciones
%
% Con el CUT en cucurucho, va a buscar el primer sabor que le guste y dejarlo fijo, y luego buscará los posibles segundos sabores
% (realizará todas las combinaciones posibles, con el primer sabor fijo)
%
% Con el CUT en leGusta y Cucurucho, buscará el primer sabor frutal que sea cremoso (CUT en leGusta), dejará ese fijo (CUT en cucurucho)
% y buscará como segundo sabor el primer sabor frutal que sea cremoso (CUT en leGusta).
%

leGusta_(X) :- frutal(X), !, cremoso(X).
cucurucho_(X, Y) :- leGusta_(X), leGusta_(Y).

leGusta__(X) :- frutal(X), cremoso(X).
cucurucho__(X, Y) :- leGusta__(X), !, leGusta__(Y).

leGusta___(X) :- frutal(X), !, cremoso(X).
cucurucho___(X, Y) :- leGusta___(X), !, leGusta___(Y).



% ------------------------------------------------
%
% EJERCICIO 17
%


% ---------------------------
%
% i. Sean los predicados P(?X) y Q(?X), ¿qué significa la respuesta a la siguiente consulta?
%
%   ?- P(Y), not(Q(Y)).
%
% La respuesta a esta consulta devolverá un Y que satisface P y no satisface Q.
%


% ---------------------------
%
% ii. ¿Qué pasaría si se invirtiera el orden de los literales en la consulta anterior?
%
% Si se invierte el orden de los predicados, va a verificar que ningún Y satisface Q
% (potencialmente iterando infinitamente) y luego tratará de encontrar un Y que satisfaga
% P.
%
% Esto ocurre porque el Y dentro del not no estaría asociado a un valor específico (hay que
% buscar uno) y además, ese valor será independiente del valor para satisfacer P
%


% ---------------------------
%
% iii. Sea el predicado P(?X), ¿Cómo se puede usar el not para determinar si existe una
% única Y tal que P(?Y) es verdadero?
%
% Se puede hacer:
%
%   ?- P(X), not(P(Y), Y \= X).
%
% Esto va a buscar un X que satisfaga P, y luego buscará un Y que satisfaga P con Y distinto de X.
% Si esto último es satisfactorio, entonces el not fallará fallando la consulta. Si no se encuentra
% un Y (distinto de X) que satisfaga P, entonces X es único.
%


% ------------------------------------------------
%
% EJERCICIO 18
%
% Definir el predicado corteMásParejo(+L,-L1,-L2) que, dada una lista de números, realiza el corte
% más parejo posible con respecto a la suma de sus elementos (puede haber más de un resultado).
% Por ejemplo:
%
%   ?- corteMásParejo([1,2,3,4,2], L1, L2).         → L1 = [1, 2, 3], L2 = [4, 2] ;
%                                                     false.
%   ?- corteMásParejo([1,2,1], L1, L2).             → L1 = [1], L2 = [2, 1] ;
%                                                     L1 = [1, 2], L2 = [1] ;
%                                                     false.
%

%! corteMásParejo(+L,-L1,-L2)
corteMásParejo(L, L1, L2) :- generaCortes(L, L1, L2, Diff), not((generaCortes(L, _, _, LDiff), LDiff < Diff)).

%! generaCortes(+L, -L1, -L2, -Diff)
generaCortes(L, L1, L2, Diff) :- append(L1, L2, L), sum_list(L1, SL1), sum_list(L2, SL2), Diff is abs(SL1 - SL2).



% ------------------------------------------------
%
% EJERCICIO 19
%
% Dado un predicado unario P(?X) sobre números naturales, definir un predicado que determine el mínimo X que
% satisfaga P(X). Se puede suponer que hay una cantidad finita de naturales que lo satisfacen.
%

%! p(+N)
p(N) :- N > 5, N * N >= 121, nth_integer_root_and_remainder(2, N, _, Remainder), Remainder is 0.

buscarMinimoN(P, N) :- desde(0, N), call(P, N), !.
buscarMinimoNSinCut(P, N) :- desde(0, N), call( P, N).

buscarMinimoNDesde(P, Inicio, N) :- desde(Inicio, N), call(P, N), !.


% ------------------------------------------------
%
% EJERCICIO 20
%
% Un número poderoso es un número natural m tal que por cada número primo p que divide a m, p*p también divide a m.
% Definir el predicado próximoNumPoderoso(+X,-Y) que instancie en Y el siguiente número poderoso a partir de X.
% Por ejemplo:
%
%   ?- próximoNumPoderoso(20,Y).
%       Y = 25;
%       false.
%
%   ?- próximoNumPoderoso(8,Y).
%       Y = 9;
%       false.
%
% Notar que, como en el último caso, si X ya es un número poderoso, Y no debe instanciarse con el valor de X, sino
% con el siguiente número poderoso.
%

%! numPoderoso(+N)
numPoderoso(N) :- divisoresPrimos(N, Primos), not((member(P, Primos), (N mod P) =:= 0, (N mod (P*P)) > 0)).

%! divisoresPrimos(+N, -Primos)
divisoresPrimos(0, []).
divisoresPrimos(1, []).
divisoresPrimos(N, Primos) :- N > 1, factorizarN(N, 2, FactoresPrimos), list_to_set(FactoresPrimos, Primos).

%! factorizarN(+N, +Start, -Primos)
factorizarN(N, Start, []) :- N < Start.
factorizarN(N, Start, Primos) :-
    N >= Start,
    (N mod Start) > 0,
    Next is Start + 1,
    factorizarN(N, Next, Primos).
factorizarN(N, Start, Primos) :-
    N >= Start,
    (N mod Start) =:= 0,
    NewN is N / Start,
    factorizarN(NewN, Start, NewPrimos),
    append([Start], NewPrimos, Primos).


%! próximoNumPoderoso(+X,-Y)
próximoNumPoderoso(X, Y) :- Inicio is X + 1, buscarMinimoNDesde(numPoderoso, Inicio, Y).


% ------------------------------------------------
%
% EJERCICIO 21
%
% Contamos con una representación de conjuntos desconocida, que permite enumerar un conjunto
% mediante el predicado pertenece(?Elemento, +Conjunto). Dado el siguiente predicado:

% -- alternativa a la definicion del ejercicio 3
natural_2(cero).
natural_2(suc(X)) :- natural_2(X).


% ---------------------------
%
% i. Definir el predicado conjuntoDeNaturales(X) que sea verdadero cuando todos los elementos
% de X son naturales (se asume que X es un conjunto).

%! conjuntoDeNaturales(+X)
conjuntoDeNaturales(X) :- not(( member(N, X), not(natural_2(N)) )).


% ---------------------------
%
% ii. ¿Con qué instanciación de X funciona bien el predicado anterior? Justificar.
%
% X es un parametro que debe estar instanciado en una lista.
% NO SE PUEDE usar el predicado para generar posibles conjuntos de naturales porque
% la búsqueda se realiza dentro de un not y eso no permite extraer valores hacia afuera.
%


% ---------------------------
%
% iii. Indicar el error en la siguiente definición alternativa, justificando por qué
% no funciona correctamente:
%
%       conjuntoDeNaturalesMalo(X) :- not( (not(natural(E)), pertenece(E,X)) ).
%
% Hay dos errores en esta definición:
%
% 1: el not interno, ver que no hay ningún natural E que pertenezca a X. Esto de por sí
%    nunca termina (hay inifinitos naturales)
% 2: una vez que se verifica el éxito del not interno, entonces falla la evaluación (no
%    se llega nunca aquí)
%






% ------------------------------------------------
% ------------------------------------------------
% ------------------------------------------------
%
% EJERCICIOS INTEGRADORES
%

% ------------------------------------------------
%
% EJERCICIO 22
%
% En este ejercicio trabajaremos con grafos no orientados. Un grafo no orientado es un conjunto de nodos y un
% conjunto de aristas sin una dirección específica. Cada arista está representada por un par de nodos y, como
% se puede viajar en cualquiera de los dos sentidos, la arista (a,b) y la arista (b,a) son la misma.
%
% No sabemos cuál es la representación interna de un grafo, pero contamos con un predicado esNodo(+G,?X) que dado
% un grafo G dice si X es nodo de G. También tenemos otro predicado esArista(+G,?X,?Y) que dice si en G hay una
% arista de X a Y. Notar que podemos usar esNodo para enumerar los nodos del grafo y esArista para enumerar las
% aristas. Instanciando apropiadamente, también podemos usar esArista para obtener todas las aristas que tienen
% a un nodo particular. Cuando esArista lista todas las aristas, cada arista se lista una sola vez en una
% orientación arbitraria de las dos posibles, pero si se pregunta por cualquiera de las dos, responderá que sí.
% Suponer que dos nodos son el mismo si y solo si unifican.
%
% Ayuda: para algunos items conviene pensar primero en cómo programar el predicado opuesto al que se pide.
%

%! esNodo(+G,?X)
esNodo(g1, X) :- member(X, [ n10, n11, n12, n13, n14, n15, n16, n17 ]).         % caso general
esNodo(g2, X) :- member(X, [ n20, n21, n22, n23, n24 ]).                        % camino de euler
esNodo(g3, X) :- member(X, [ n30, n31, n32, n33, n34, n335 ]).                  % n30..n33 es completo, n34+35 separado
esNodo(g4, X) :- member(X, [ n40, n41, n42, n43, n44 ]).                        % estrella (n42 es centro)
esNodo(g5, X) :- member(X, [ n20, n21, n22, n23 ]).                             % K-4


%! esArista(+G,?X,?Y)
esArista(g1, X, Y) :-
    aristas(X, Y, [ (n10, n12), (n13, n10), (n11, n13), (n11, n14), (n12, n14), (n12, n15), (n13, n15), (n13, n16), (n14, n16), (n14, n17), (n15, n17), (n15, n10), (n16, n10), (n16, n11), (n17, n11), (n17, n12) ]).
esArista(g2, X, Y) :-
    aristas(X, Y, [ (n20, n21), (n21, n22), (n22, n23), (n23, n20), (n22, n20), (n21, n23), (n20, n24), (n24, n23) ]).
esArista(g3, X, Y) :-
    aristas(X, Y, [ (n30, n31), (n31, n32), (n32, n33), (n33, n30), (n31, n33), (n30, n32), (n34, n35) ]).
esArista(g4, X, Y) :-
    aristas(X, Y, [ (n40, n42), (n42, n41), (n42, n43), (n42, n44) ]).
esArista(g5, X, Y) :-
    aristas(X, Y, [ (n20, n21), (n21, n22), (n22, n23), (n23, n20), (n22, n20), (n21, n23) ]).

%! aristas(?X, ?Y, +Aristas).
aristas(X, Y, Aristas) :-
    member( (N1, N2), Aristas),
    arista_match(X, Y, N1, N2).
arista_match(X, Y, N1, N2) :- X = N1, Y = N2, !.
arista_match(X, Y, N1, N2) :- X = N2, Y = N1, !.


% ---------------------------
%
% i. Implementar el predicado caminoSimple(+G,+D,+H,?L) que dice si L es un camino simple en el grafo G que empieza
% en D y termina en H. Un camino simple lo representaremos por una lista de nodos distintos, tal que para cada par
% de nodos consecutivos en L existe una arista en G que los conecta. Notar que el primer elemento de L debe ser D y
% el último H. Cuando L está sin instanciar, el predicado debe ir devolviendo todos los caminos simples desde D a H
% sin repetidos (es decir, hay que tener cuidado con los ciclos).

%! caminoSimple(+G, +D, +H, ?L)
caminoSimple(G, D, D, []) :- esNodo(G, D).
caminoSimple(G, D, H, L) :- esNodo(G, D), esNodo(G, H), D \= H, caminoSimple(G, D, H, [], L).

%
% Sabemos que D y H son nodos del grafo.
%
%! caminoSimple(+G, +D, +H, +Visitados, -Camino)
caminoSimple(_, D, D, _, [D]).
caminoSimple(G, D, H, Visitados, Camino) :-
    % excluyente de la terminación
    D \= H,
    % nos consideramos visitados
    append(Visitados, [D], NuevoVisitados),
    % buscamos una nueva arista, verificamos que no haya sido visitada y vamos a ese punto
    esArista(G, D, I),
    not(member(I, NuevoVisitados)),
    % tratamos de completar el camino
    caminoSimple(G, I, H, NuevoVisitados, CaminoResto),
    % agregamos nuestro nodo al camino que obtuvimos
    append([D], CaminoResto, Camino).


% ---------------------------
%
% ii. Un camino L en un grafo G es Hamiltoniano sii L es un camino simple que contiene a todos los nodos G. Implementar
% el predicado caminoHamiltoniano(+G,?L) que dice si L es un camino Hamiltoniano en G.

%! caminoHamiltoniano(+G,?Path)
caminoHamiltoniano(G, Path) :-
    nonvar(Path),
    % verificamos que todos los elementos de path sean nodos del grafo
    not(( member(Node, Path), not(esNodo(G, Node)) )),
    % verificamos que todos los nodos del grafo estén presentes en el camino
    grafoTodosLosNodos(G, Path),
    % verificamos que el camino exista en el grafo
    prefix([D], Path),
    append(_, [H], Path),
    caminoSimple(G, D, H, Path).
caminoHamiltoniano(G, Path) :-
    var(Path),
    % elegimos un par de nodos (y vemos que sean diferentes)
    esNodo(G, Desde),
    esNodo(G, Hasta),
    Desde \= Hasta,
    % elegimos un camino y verificamos que cubra todos los nodos
    caminoSimple(G, Desde, Hasta, Path),
    grafoTodosLosNodos(G, Path).


%! grafoTodosLosNodos(+G, +Lista)
grafoTodosLosNodos(G, Lista) :- 
    not(( esNodo(G, Node), not(member(Node, Lista)) )).


% ---------------------------
%
% iii. Implementar el predicado esConexo(+G) que dado un grafo dice si este es conexo. Un grafo G es conexo sii no
% existe un par de nodos en G tal que no hay un camino simple que los una. Notar que con esta definición un grafo
% de un nodo (y sin aristas) es conexo.

%! esConexo(+G)
esConexo(G) :-
    not((
        esNodo(G, NOri),
        esNodo(G, NDest),
        NOri \= NDest,
        not(caminoSimple(G, NOri, NDest, _))
    )).



% ---------------------------
%
% iv. Implementar el predicado esEstrella(+G) que dado un grafo dice si es un grafo estrella. Un grafo es estrella
% sii es conexo y hay un nodo común a todas sus aristas.

%! esEstrella(+G)
esEstrella(G) :-
    esConexo(G),
    posibleCentro(G, Centro),
    not((posibleCentro(G, OtroCentro), Centro \= OtroCentro)).

%! posibleCentro(G, Centro)
posibleCentro(G, Centro) :-
    esNodo(G, Centro),
    not((esNodo(G, N1), N1 \= Centro, esArista(G, N1, N2), N1 \= N2, N2 \= Centro)),
    !.



% ------------------------------------------------
%
% EJERCICIO 23
%
% Trabajaremos con árboles binarios, usando nil y bin(AI, V, AD) para representarlos en Prolog.
%



% ---------------------------
%
% i. Implementar un predicado arbol(-A) que genere estructuras de árbol binario, dejando los
% valores de los nodos sin instanciar. Deben devolverse todos los árboles posibles (es decir,
% para toda estructura posible, el predicado debe devolverla luego de un número finito de
% pedidos). No debe devolverse dos veces el mismo árbol.
%
%   ?- arbol(A).
%   A = nil ;
%   A = bin(nil, _G2388, nil) ;
%   A = bin(nil, _G2391, bin(nil, _G2398, nil)) ;
%   A = bin(bin(nil, _G2398, nil), _G2391, nil) ;
%   ...
%

%! arbol(-A)
arbol(A) :-
    desde(0, CantNodos),
    generarArbol(CantNodos, A).

%! generarArbol(+CantNodos, -A)
generarArbol(0, nil).
generarArbol(1, bin(nil, _, nil)).
generarArbol(CantNodos, A) :-
    CantNodos > 1,
    CantMax is CantNodos - 1,
    between(0, CantMax, NodosIzq),
    NodosDer is CantMax - NodosIzq,
    generarArbol(NodosIzq, AIzq),
    generarArbol(NodosDer, ADer),
    A = bin(AIzq, _, ADer).


% ---------------------------
%
% ii. Implementar un predicado nodosEn(?A, +L) que es verdadero cuando A es un árbol cuyos nodos
% pertenecen al conjunto conjunto de átomos L (representado mediante una lista no vacía, sin orden
% relevante y sin repetidos). Puede asumirse que el árbol se recibe instanciado en su estructura,
% pero no necesariamente en sus nodos.
%
%   ?- arbol(A), nodosEn(A, [ka, pow]).
%   A = nil ;
%   A = bin(nil, ka, nil) ;
%   A = bin(nil, pow, nil) ;
%   A = bin(nil, ka, bin(nil, ka, nil)) ;
%   A = bin(nil, ka, bin(nil, pow, nil)) ;
%   ...

%! nodosEn(?A, +L)
nodosEn(nil, _).
nodosEn(bin(Izq, R, Der), L) :-
    nodosEn(Izq, L),
    nodosEn(Der, L),
    member(R, L).


% ---------------------------
%
% iii. Implementar un predicado sinRepEn(-A, +L) que genere todos los árboles cuyos nodos
% pertenezcan al alfabeto L y usando como máximo una vez cada símbolo del mismo. En este caso,
% no hay infinitos árboles posibles; es importante que el predicado no devuelva soluciones
% repetidas y que no se quede buscando indefinidamente una vez terminado el espacio de soluciones.
%
%   ? arbolSinRepEn(A, [ka, pow]).
%   A = nil ;
%   A = bin(nil, ka, nil) ;
%   A = bin(nil, pow, nil) ;
%   A = bin(nil, ka, bin(nil, pow, nil)) ;
%   A = bin(nil, pow, bin(nil, ka, nil)) ;
%   ... ;
%   No.
%

%! arbolSinRepEn(-A, +L)
arbolSinRepEn(A, L) :-
    length(L, MaxCantNodos),
    between(0, MaxCantNodos, CantNodos),
    generarArbolSinRep(CantNodos, L, _, A).

%! generarArbolSinRep(+CantNodos, +L, -A)
generarArbolSinRep(0, Disp, Disp, nil).
generarArbolSinRep(1, Disp, Usados, bin(nil, R, nil)) :-
    member(R, Disp),
    delete(Disp, R, Usados).
generarArbolSinRep(CantNodos, Disp, Usados, A) :-
    CantNodos > 1,
    CantMax is CantNodos - 1,
    between(0, CantMax, NodosIzq),
    NodosDer is CantMax - NodosIzq,
    generarArbolSinRep(NodosIzq, Disp, UsadosIzq, AIzq),
    generarArbolSinRep(NodosDer, UsadosIzq, UsadosDer, ADer),
    member(R, UsadosDer),
    delete(UsadosDer, R, Usados),
    A = bin(AIzq, R, ADer).

