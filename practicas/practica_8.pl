
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
reverse_p8([], []).
reverse_p8(L, R) :- append([H], T, L), reverse_p8(T, TR), append(TR, [H], R).

/*
?-  reverse_p8([a,b,c], [c,b,a]).
    |
    +-- { L := [a,b,c], R := [c,b,a] }
        append([H1], T1, [a,b,c]), reverse_p8(T1, TR1), append(TR1, [H1], [c,b,a])
        |
        +-- { H1 := a, T1 := O1, L1 := [b,c] }
            append([], O1, [b,c]), reverse_p8(T2, TR1), append(TR1, [a], [c,b,a])
            |
            +-- { L21 := [b,c] }
                reverse_p8([b,c], TR2), append(TR2, [a], [c,b,a])
                |
                +-- { TR2 := L, L := [b,c] }
                    append([H2], T2, [b,c]), reverse_p8(T2, TR2), append(TR2, [H2], TR1), append(TR1, [a], [c,b,a])
                    |
                    +-- { H2:= b, T2 := L23, L1 := [c] }
                        append([], L23, [c]), reverse_p8(L23, TR2), append(TR2, [b], TR1), append(TR1, [a], [c,b,a])
                        |
                        +-- { L23 := [c] }
                            reverse_p8([c], TR3), append(TR2, [b], TR1), append(TR1, [a], [c,b,a])
                            |
                            +-- { H3 := c, T3 := L23, L := [] }
                                append([c], L23, [c]), reverse_p8(T3, TR3), append(TR3, [c], TR3), append(TR, [b], R), append(TR, [a], [c,b,a])

append([], L, L).
append([H|T], O, [H|R]) :- append(T, O, L).
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
% Definir los siguientes predicados, usando member y/o append según sea conveniente:



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
reparto(L, N, LListas) :- N > 0, append(Parte, Resto, L), N1 is N-1, reparto(Resto, N1, LListasRec), append([Parte], LListasRec, LListas).

% ---------------------------
%
% vi. repartoSinVacías(+L, -LListas) similar al anterior, pero ninguna de las listas de
% LListas puede ser vacía, y la longitud de LListas puede variar.


%! repartoSinVacías(+L, -LListas)
repartoSinVacías([], []).
repartoSinVacías(L, LListas) :- append(Parte, Resto, L), Parte \= [], repartoSinVacías(Resto, LListasRec), append([Parte], LListasRec, LListas).





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

