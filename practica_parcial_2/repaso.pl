% ------------------------------------------------------------------------
%
% REPASO 2DO PARCIAL
%
% ------------------------------------------------------------------------

% ------------------------------------
% 
% Ejercicio 1 - Programación Lógica
%
% Implementar los predicados respetando en cada caso la instanciación pedida. Los generadores deben cubrir todas las instancias
% válidas de aquello que generan sin repetir dos veces la misma. Se deben indicar los patrones de instanciación de todos los
% predicados auxiliares. No usar macros que colapsen cláusulas, cut (!) ni predicados de alto orden, con la única excepción de
% not. No está permitido el uso de estructuras auxiliares.
%

% ------------------
% 
% a) Definir el predicado unico(+L,-U) que es verdadero cuando U aparece una sola vez en la lista L. Resolver sin contar
% apariciones. Por ejemplo:
%
%   ?- unico([a,b,c,a,a,b,d], U).
%   U = c ;
%   U = d ;
%   false.
%

%! unico(+L,-U)
unico(L, U) :-
    append(Prefijo, [U|Sufijo], L),
    append(Prefijo, Sufijo, Resto),
    not(member(U, Resto)).


% ------------------
% 
% b) Definir el predicado sinRepetidos(+L) que represente la siguiente propiedad: una lista L hace verdadero a sinRepetidos
% si todos sus elementos son únicos.
%

%! sinRepetidos(+L)
sinRepetidos(L) :-
    not(( member(U, L), not(unico(L, U)) )).


% ------------------
% 
% c) En este inciso vamos a representar a un subconjunto de las fórmulas de lógica proposicional con negación e implicación.
% Los átomos utilizados serán neg(P) e imp(P,Q), con P y Q fórmulas. Definir el predicado formula(+VS, -F) que instancia en F
% todas las fórmulas que usan las variables proposicionales que aparecen en la lista VS. Por ejemplo:
%
%   ?- formula([p,q],F).
%   F = p;
%   F = q;
%   F = neg(p);
%   F = neg(q);
%   F = neg(neg(p));
%   F = neg(neg(q));
%   F = imp(p,p);
%   F = imp(p,q);
%   ...
%   F = neg(imp(q,q));
%   F = imp(p,neg(p));
%   ...
%   F = imp(neg(q),imp(neg(p),p));
%   ...
%
% Sugerencia: prestar atención al orden en que se devuelven los resultados. No es necesario que se mantenga el orden del
% ejemplo, pero sí que se instancien todas las fórmulas.
%

%! generarFormulas(+N, +VS, -F)
generarFormulas(0, VS, F) :-
    member(F, VS).
generarFormulas(N, VS, neg(FBase)) :-
    N > 0,
    NM1 is N - 1,
    generarFormulas(NM1, VS, FBase).
generarFormulas(N, VS, imp(FIzq, FDer)) :-
    N > 1,
    NM1 is N - 1,
    between(0, NM1, CantIzq),
    CantDer is NM1 - CantIzq,
    generarFormulas(CantIzq, VS, FIzq),
    generarFormulas(CantDer, VS, FDer).

desde(X, X).
desde(X, Y) :- N is X + 1, desde(N, Y).

%! formula(+VS, -F)
formula(VS, F) :-
    desde(0, N),
    generarFormulas(N, VS, F).


% ------------------
% 
% d) ¿El predicado anterior es reversible en F? Justificar brevemente.
%

%
% No es reversible. Al asignar F eventualmente va a encontrar el N (cantidad de nodos) apropiado y con eso va a poder
% generar la formula, indicando true. El problema es que al pedir otro valor, va a seguir buscando, con N (cantidad de
% nodos) cada vez más grande, sin poder generar la fórmula nuevamente y por lo tanto se va a colgar.
%
