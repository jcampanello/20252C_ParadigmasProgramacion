% ------------------------------------------------------------------------
%
% 2025-1C
%
% ------------------------------------------------------------------------

% ------------------------------------
% 
% Ejercicio 1 - Programación Lógica
%
% Implementar los predicados respetando en cada caso la instanciación pedida. Los generadores deben cubrir
% todas las instancias válidas de aquello que generan sin repetir dos veces la misma. Se deben indicar los
% patrones de instanciación de todos los predicados auxiliares. No usar cut (!) ni predicados de alto orden
% como setof, con la única excepción de not. No está permitido usar macros que colapsen cláusulas.
%

% ------------------
% 
% a) Definir el predicado esRotación(+L,+R) que es verdadero cuando R es una rotación de L. Una lista R es
% una rotación de una lista L si contiene los mismos elementos en el mismo orden relativo, pero comenzando
% desde una posición diferente dentro de L, recorriendo la lista circularmente. Por ejemplo:
%
%   ?- esRotacion([1,2,3,4], [3,4,1,2]).
%   true.
%   ?- esRotacion([a,b,c], [c,a,b]).
%   true.
%   ?- esRotacion([a,b,c], [c,b,a]).
%   false.
%

%! esRotacion(+L, +R)
esRotacion([], []).
esRotacion(L, R) :-
    append([H|Parte1], Parte2, L),
    append(Parte2, [H|Parte1], R).


% ------------------
% 
% b) La secuencia de Collatz se construye con la siguiente regla sobre un número natural: si n es par, el
% siguiente número es n/2; si n es impar, el siguiente número es 3n + 1. La secuencia termina cuando se
% llega al número 1 (aunque la conjetura dice que siempre se llega a 1, esto no ha sido probado para todos
% los naturales).
% Definir el predicado collatz(+N,-S) que instancia en S todos los números pertenecientes a la secuencia
% de Collatz a partir del número N. Por ejemplo:
%
%   ?- collatz(6, S).
%   S = 6;
%   S = 3;
%   S = 10;
%   S = 5;
%   S = 16;
%   S = 8;
%   S = 4;
%   S = 2;
%   S = 1;
%   false.
%

%! collatz(+N, -S)
collatz(N, N).
collatz(N, S) :- N > 1, N mod 2 =:= 0, N1 is N / 2, collatz(N1, S).
collatz(N, S) :- N > 1, N mod 2 =:= 1, N1 is N * 3 + 1, collatz(N1, S).



% ------------------
% 
% c) ¿El predicado anterior es reversible en S? (pueden asumir que se utilizarán números bajos por lo que la
% secuencia es siempre finita). Justificar brevemente.
%

%
% Si, el predicado es reversible en S. Si se le indica un valor de S (por ej: 10), la primer regla no será
% match e intentará entrar por alguna de las otras dos, calculando el siguiente número de N y generando
% un nuevo término a evaluar de collatz, pero ahora con un nuevo N y el mismo S.
% Esto repetirá la cuenta hasta tanto pueda pueda entrar en la primera regla (y terminar el ciclo), momento
% en el cual deberá unificar N (1, para la terminación) con S y se obtendrá true o false.
%
%


% ------------------
% 
% d) Definir el predicado collatzMayor(+N,-M) que es verdadero cuando M es el número más grande de la secuencia
% de Collatz a partir del número N. Por ejemplo:
%
%   ?- collatzMayor(6, M).
%   M = 16 ;
%   false.
%

%! collatzMayor(+N, -M)
collatzMayor(N, M) :-
    collatz(N, M),
    not((collatz(N, S), M < S)).

