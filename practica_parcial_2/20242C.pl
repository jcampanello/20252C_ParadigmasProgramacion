% ------------------------------------------------------------------------
%
% 2024-2C
%
% ------------------------------------------------------------------------

% ------------------------------------
% 
% Ejercicio 1 - Programación Lógica
%
% Implementar los predicados respetando en cada caso la instanciación pedida. Los generadores deben cubrir todas las
% instancias válidas de aquello que generan sin repetir dos veces la misma. Se deben indicar los patrones de
% instanciación de todos los predicados auxiliares. No usar cut (!) ni predicados de alto orden como setof, con la
% única excepción de not.
%

% ------------------
% 
% a) Definir el predicado subsecuenciaCreciente(+L,-S) que es verdadero cuando S es una subsecuencia estrictamente
% creciente de elementos de L. Notar que la secuencia respeta el orden de aparición en L. Por ejemplo:
%
%   ?- subsecuenciaCreciente([4,8,1,9], S).
%   S = [4, 8, 9] ;
%   S = [4, 8] ;
%   S = [4, 9] ;
%   S = [4] ;
%   S = [8, 9] ;
%   S = [8] ;
%   S = [1, 9] ;
%   S = [1] ;
%   S = [9] ;
%   S = [].

%! subsecuenciaCreciente(+L, -S)
subsecuenciaCreciente(L, S) :-
    generarSublistas(L, S),
    testearSublista(S).

%! generarSublistas(+L, -S)
generarSublistas([], []).
generarSublistas([H|T], S) :-
    generarSublistas(T, S1),
    append([H], S1, S).
generarSublistas([_|T], S) :-
    generarSublistas(T, S).


%! testearSublista(+S)
testearSublista().
testearSublista([_]).
testearSublista([H1,H2|T]) :-
    H1 < H2,
    testearSublista([H2|T]).

% ------------------
% 
% b) Definir el predicado subsecuenciaCrecienteMasLarga(+L, -S) que es verdadero cuando S es la subsecuencia
% estrictamente creciente de mayor longitud de L. Puede haber más de un resultado. Por ejemplo:
%
%   ?- subsecuenciaCrecienteMasLarga([5,6,7,2,8,1,2,3,4,5,7], S).
%   S = [1,2,3,4,5,7] ;
%   false;
%   
%   ?- subsecuenciaCrecienteMasLarga([5,6,7,2,8,0,2,3,4], S).
%   S = [5,6,7,8] ;
%   S = [0,2,3,4] ;
%   false.
%

%! subsecuenciaCrecienteMasLarga(+L, -S)
subsecuenciaCrecienteMasLarga(L, S) :-
    subsecuenciaCreciente(L, S),
    length(S, SLen),
    not((subsecuenciaCreciente(L, S1), length(S1, S1Len), S1Len > SLen)).


% ------------------
% 
% c) Definir el predicado fibonacci(-X) que instancia en X los números pertenecientes a la secuencia de
% Fibonacci. Por ejemplo:
%
%   ?- fibonacci(X).
%   X = 1;
%   X = 1;
%   X = 2;
%   X = 3;
%   X = 5;
%   ...
%

%! fibonacci(-X)
fibonacci(1).
fibonacci(N) :- generarFibonacci(0, 1, N).

%! generarFibonacci(+PrevPrev, +Prev, N).
generarFibonacci(PrevPrev, Prev, N) :- N is PrevPrev + Prev.
generarFibonacci(PrevPrev, Prev, N) :- PrevProx is PrevPrev + Prev, generarFibonacci(Prev, PrevProx, N).



% ------------------
% 
% d) ¿Es reversible el predicado anterior? Justificar.
%

%
% El predicado no es reversible. Si se pasa un N específico (que es un número de fibonacci), el predicado
% primero indicará true pero al pedir el siguiente valor, continuará generando números de la serie,
% para ver si llega al número instanciado.
% Si se pasa un número que no pertenece a la serie, se quedará buscando infinitamente para ver si
% ese número pertenece a la serie.
%
