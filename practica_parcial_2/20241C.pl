% ------------------------------------------------------------------------
%
% 2024-1C
%
% ------------------------------------------------------------------------

% ------------------------------------
% 
% Ejercicio 1 - Programación Lógica
%
% Implementar los predicados respetando en cada caso la instanciación pedida. Los generadores deben cubrir todas las
% instancias válidas de aquello que generan sin repetir dos veces la misma. No usar cut (!) ni predicados de alto
% orden como setof, con la única excepción de not.
%
% En este ejercicio ayudaremos a un grupo de jurados a analizar las notas de las materias que cursaron algunos
% estudiantes para presentarse a concurso. Se cuenta con el predicado estudiante(?E) que es verdadero cuando E
% es un estudiante, y con el predicado notas(-XS) que instancia en XS una lista de triplas correspondientes a
% las notas de los estudiantes en cada materia de la siguiente forma: (Estudiante, Materia, Nota). En la lista
% de notas pueden aparecer aplazos, pero se asume que está bien formada (es decir, puede haber muchos aplazos
% para un estudiante en una materia, pero a lo sumo un aprobado). Se pide:
%

estudiante(e1).
estudiante(e2).
estudiante(e3).
estudiante(e4).

notas((e1, m1, 2)).
notas((e1, m1, 9)).
notas((e1, m2, 6)).
notas((e1, m3, 2)).
notas((e1, m3, 4)).
notas((e2, m1, 5)).
notas((e2, m3, 6)).
notas((e2, m5, 7)).
notas((e2, m6, 1)).
notas((e3, m2, 8)).
notas((e4, m2, 10)).
notas((e4, m3, 6)).


% ------------------
% 
% a) Definir el predicado tieneMateriaAprobada(+E,+M) que es verdadero cuando el estudiante E tiene la materia M
% aprobada (es decir, la nota es mayor o igual a 4).

%! tieneMateriaAprobada(+E, +M)
tieneMateriaAprobada(E, M) :-
    estudiante(E),
    not( not((notasEstudianteMateria(E, M, Nota), Nota = (_, _, Puntaje), Puntaje >= 4)) ).

notasEstudianteMateria(E, M, Nota) :-
    notas(Nota),
    Nota = (Estudiante, Materia, _),
    E = Estudiante,
    M = Materia.



% ------------------
% 
% b) Definir el predicado eliminarAplazos(+NS,-L) que es verdadero cuando NS es una lista de notas y L es la misma
% lista, pero eliminando los aplazos. Los aplazos sólo pueden eliminarse si el estudiante finalmente aprobó la
% materia. Por ejemplo:
%
%   ?- eliminarAplazos([ (juan,plp,3), (juan,plp,9), (maria,tlen,2) ], L).
%   L = [ (juan,plp,9), (maria,tlen,2) ].
%

%
% IMPORTANTE: aqui usaremos el hecho de que la lista que recibimos es independiente de las notas y por lo tanto
%             podemos usar el predicado tieneMateriaAprobada (que usa notas, independiente de la lista NS)
%


%! eliminarAplazos(+NS, -L)
eliminarAplazos([], []).
eliminarAplazos([H|T], L) :-
    % identificamos estudiante y materia y si tiene aprobada
    H = (E, M, P),
    tieneMateriaAprobada(E, M),
    % si la nota es un aplazo la ignoramos y seguimos
    P < 4,
    eliminarAplazos(T, L).
eliminarAplazos([H|T], L) :-
    % identificamos estudiante y materia y si tiene aprobada
    H = (E, M, P),
    tieneMateriaAprobada(E, M),
    % si la nota es aprobado, filtramos sobre el resto
    P >= 4,
    eliminarAplazos(T, TFiltrada),
    % esta nota va porque no era un aplazo
    append([H], TFiltrada, L).
eliminarAplazos([H|T], L) :-
    % identificamos estudiante y materia y si NOT tiene aprobada
    H = (E, M, _),
    not(tieneMateriaAprobada(E, M)),
    % filtramos sobre el resto
    eliminarAplazos(T, TFiltrada),
    % esta nota va porque no tiene aprobada la materia
    append([H], TFiltrada, L).


% ------------------
% 
% c) Definir el predicado promedio(+A,-P) que es verdadero cuando A es un estudiante, y P es el promedio de todas
% sus notas luego de eliminar los aplazos.
%
% Sugerencia: armar una lista de notas de A.
%

%! promedio(+A, -P)
promedio(A, P) :-
    % obtenemos las notas y filtramos aplazos (de materias aprobadas)
    listaNotas(A, Notas),
    eliminarAplazos(Notas, NotasFiltradas),
    % calculamos suma de notas y cantidad de materias
    extraerNotas(NotasFiltradas, Puntajes),
    sum_list(Puntajes, Puntaje),
    length(Puntajes, CantNotas),
    % calculamos el puntaje. El max cubre el caso de lista vacia y evita
    % division por 0 (cantidad) pero en cuyo caso el puntaje sería 0,
    % con lo cual se obtiene 0 de todas formas
    P is Puntaje / max(CantNotas, 1).

%! listaNotas(+E, -L)
listaNotas(E, L) :-
    findall(N, ((notas(N), N = (E1, _, _), E = E1)), L).

%! extraerNotas(+Notas, -Puntajes)
extraerNotas([], []).
extraerNotas([H|T], Puntajes) :-
    extraerNotas(T, TPuntajes),
    H = (_, _, Puntaje),
    append([Puntaje], TPuntajes, Puntajes).



% ------------------
% 
% d) Definir el predicado mejorEstudiante(-A) que es verdadero cuando A es el estudiante cuyo promedio es el más alto.
% Puede haber más de una solución en caso de que haya más de un estudiante con el promedio más alto. En este inciso no
% está permitido utilizar estructuras auxiliares.
%

%! mejorEstudiante(-E)
mejorEstudiante(E) :-
    estudiante(E),
    promedio(E, P),
    not((estudiante(OtroE), promedio(OtroE, OtroP), OtroP > P)).



