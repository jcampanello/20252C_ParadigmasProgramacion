

% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 1

%! matriz(+F, +C, -M)
matriz(F, C, M) :-
	length(M, F),
	length(Sample_Row, C),
	maplist(same_length(Sample_Row), M).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 2

%! replicar(+X, +N, -L)
replicar(X, N, L) :- length(L, N), include(=(X), L, L).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 3
%! transponer(+M, -MT)
transponer(M, MT) :-
	nth1(1, M, FirstRow),
	length(FirstRow, ColCount),
	replicar([], ColCount, MT0),
	foldl(agregar_a_columna, M, MT0, MT).
agregar_a_columna([], [], []).
agregar_a_columna([HR | TR], [HM0 | TM0], [HMT | TMT]) :-
	agregar_a_columna(TR, TM0, TMT),
	append(HM0, [HR], HMT).



% Predicado dado armarNono/3
armarNono(RF, RC, nono(M, RS)) :-
	length(RF, F),
	length(RC, C),
	matriz(F, C, M),
	transponer(M, Mt),
	zipR(RF, M, RSFilas),
	zipR(RC, Mt, RSColumnas),
	append(RSFilas, RSColumnas, RS).

zipR([], [], []).
zipR([R|RT], [L|LT], [r(R,L)|T]) :- zipR(RT, LT, T).



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 4

%! pintadasValidas(+R)
pintadasValidas(r(Restric, Linea)) :- 
	length(Linea, LTotal),
	length(Restric, LRestric),
	suma(Restric, CPintada),
	CLibre is LTotal - CPintada,
	CLibre >= 0,
	Rango is max(CLibre - max(LRestric - 1, 0), 1),
	espaciosInicios(LRestric, ListaEspacios),
	generarPosibles(ListaEspacios, Restric, Rango, LTotal, Posible),
	Posible = Linea.

% AUXILIAR - sumatoria de elementos (ENTEROS) de una lista
%! suma(+L, ?Total)
suma([], 0).
suma([ Head | Tail ], Total) :- suma(Tail, Subtotal), Total is Subtotal + Head.

% AUXILIAR - arma una lista conteniendo el punto de inicio para cada espacio
% (0 para inicial/final y 1 para el resto)
%! espaciosInicios(+CantRestric, -Espacios)
espaciosInicios(0, [ 0 ]).
espaciosInicios(LRestrict, Espacios) :-
	LRestrict > 0,
	Internos is LRestrict - 1,
	replicar(1, Internos, EspaciosInternos),
	append( [ 0 ], EspaciosInternos, EspaciosIniciales),
	append(EspaciosIniciales, [ 0 ], Espacios).

% AUXILIAR - genera una posible combinacion para la linea. Recibe:
% - lista de puntos de inicio para los espacios (0 o 1), que pueden crecre hasta el valor de Rango
% - lista de pintadas (las longitudes a pintar)
% - longitud maxima de un segmento de espacios
% - longitud máxima de la línea
% - posible línea
%! generarPosibles(+ListaEspacios, +ListaPintadas, +MaximoEspacio, +Longitud, -Posible).
generarPosibles( [ X ], [ ], MaximoEspacio, _, Posible) :-
	espaciosPrevios(X, MaximoEspacio, Posible).
generarPosibles( [ HE | TE ], [ HR | TR ], MaximoEspacio, Longitud, Posible) :-
	generarPosibles(TE, TR, MaximoEspacio, Longitud, Resto),
	replicar(x, HR, Pintadas),
	append(Pintadas, Resto, PosibleTail),
	length(PosibleTail, LPosibleTail),
	LPosibleTail + HE =< Longitud,
	espaciosPrevios(HE, MaximoEspacio, Espacios),
	append(Espacios, PosibleTail, Posible),
	length(Posible, LPosible),
	LPosible =< Longitud.

% genera las posibilidades de espacios previos. Toma una cantidad inicial y una cantidad maxima,
% calcula como los posibles prefijos de la lista de cantidad maxima de espacios
%! espaciosPrevios(+CantInicial, +CantMaxima, ?Espacios)
espaciosPrevios(CantInicial, CantMaxima, Espacios) :-
	replicar(o, CantMaxima, EspaciosMaximos),
	append(Espacios, _, EspaciosMaximos),
	length(Espacios, LEspacios),
	LEspacios >= CantInicial.



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 5
%! resolverNaive(+NN)
resolverNaive(nono(M, RS)) :- maplist(pintadasValidas, RS).



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 6
pintarObligatorias(_) :- completar("Ejercicio 6").


% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 7
deducir1Pasada(_) :- completar("Ejercicio 7").


% Predicado dado
cantidadVariablesLibres(T, N) :- term_variables(T, LV), length(LV, N).

% Predicado dado
deducirVariasPasadas(NN) :-
	NN = nono(M,_),
	cantidadVariablesLibres(M, VI), % VI = cantidad de celdas sin instanciar en M en este punto
	deducir1Pasada(NN),
	cantidadVariablesLibres(M, VF), % VF = cantidad de celdas sin instanciar en M en este punto
	deducirVariasPasadasCont(NN, VI, VF).

% Predicado dado
deducirVariasPasadasCont(_, A, A). % Si VI = VF entonces no hubo más cambios y frenamos.
deducirVariasPasadasCont(NN, A, B) :- A =\= B, deducirVariasPasadas(NN).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 8
restriccionConMenosLibres(_, _) :- completar("Ejercicio 8").


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 9
resolverDeduciendo(NN) :- completar("Ejercicio 9").


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 10
solucionUnica(NN) :- completar("Ejercicio 10").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Ejemplos de nonogramas    %
%        NO MODIFICAR          %
%    pero se pueden agregar    %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fáciles
nn(0, NN) :- armarNono([[1],[2]],[[],[2],[1]], NN).
nn(1, NN) :- armarNono([[4],[2,1],[2,1],[1,1],[1]],[[4],[3],[1],[2],[3]], NN).
nn(2, NN) :- armarNono([[4],[3,1],[1,1],[1],[1,1]],[[4],[2],[2],[1],[3,1]], NN).
nn(3, NN) :- armarNono([[2,1],[4],[3,1],[3],[3,3],[2,1],[2,1],[4],[4,4],[4,2]], [[1,2,1],[1,1,2,2],[2,3],[1,3,3],[1,1,1,1],[2,1,1],[1,1,2],[2,1,1,2],[1,1,1],[1]], NN).
nn(4, NN) :- armarNono([[1, 1], [5], [5], [3], [1]], [[2], [4], [4], [4], [2]], NN).
nn(5, NN) :- armarNono([[], [1, 1], [], [1, 1], [3]], [[1], [1, 1], [1], [1, 1], [1]], NN).
nn(6, NN) :- armarNono([[5], [1], [1], [1], [5]], [[1, 1], [2, 2], [1, 1, 1], [1, 1], [1, 1]], NN).
nn(7, NN) :- armarNono([[1, 1], [4], [1, 3, 1], [5, 1], [3, 2], [4, 2], [5, 1], [6, 1], [2, 3, 2], [2, 6]], [[2, 1], [1, 2, 3], [9], [7, 1], [4, 5], [5], [4], [2, 1], [1, 2, 2], [4]], NN).
nn(8, NN) :- armarNono([[5], [1, 1], [1, 1, 1], [5], [7], [8, 1], [1, 8], [1, 7], [2, 5], [7]], [[4], [2, 2, 2], [1, 4, 1], [1, 5, 1], [1, 8], [1, 7], [1, 7], [2, 6], [3], [3]], NN).
nn(9, NN) :- armarNono([[4], [1, 3], [2, 2], [1, 1, 1], [3]], [[3], [1, 1, 1], [2, 2], [3, 1], [4]], NN).
nn(10, NN) :- armarNono([[1], [1], [1], [1, 1], [1, 1]], [[1, 1], [1, 1], [1], [1], [ 1]], NN).
nn(11, NN) :- armarNono([[1, 1, 1, 1], [3, 3], [1, 1], [1, 1, 1, 1], [8], [6], [10], [6], [2, 4, 2], [1, 1]], [[2, 1, 2], [4, 1, 1], [2, 4], [6], [5], [5], [6], [2, 4], [4, 1, 1], [2, 1, 2]], NN).
nn(12, NN) :- armarNono([[9], [1, 1, 1, 1], [10], [2, 1, 1], [1, 1, 1, 1], [1, 10], [1, 1, 1], [1, 1, 1], [1, 1, 1, 1, 1], [1, 9], [1, 2, 1, 1, 2], [2, 1, 1, 1, 1], [2, 1, 3, 1], [3, 1], [10]], [[], [9], [2, 2], [3, 1, 2], [1, 2, 1, 2], [3, 11], [1, 1, 1, 2, 1], [1, 1, 1, 1, 1, 1], [3, 1, 3, 1, 1], [1, 1, 1, 1, 1, 1], [1, 1, 1, 3, 1, 1], [3, 1, 1, 1, 1], [1, 1, 2, 1], [11], []], NN).
nn(13, NN) :- armarNono([[2], [1,1], [1,1], [1,1], [1], [], [2], [1,1], [1,1], [1,1], [1]], [[1], [1,3], [3,1,1], [1,1,3], [3]], NN).
nn(14, NN) :- armarNono([[1,1], [1,1], [1,1], [2]], [[2], [1,1], [1,1], [1,1]], NN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Predicados auxiliares     %
%        NO MODIFICAR          %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! completar(+S)
%
% Indica que se debe completar el predicado. Siempre falla.
completar(S) :- write("COMPLETAR: "), write(S), nl, fail.

%! mostrarNono(+NN)
%
% Muestra una estructura nono(...) en pantalla
% Las celdas x (pintadas) se muestran como ██.
% Las o (no pintasdas) se muestran como ░░.
% Las no instanciadas se muestran como ¿?.
mostrarNono(nono(M,_)) :- mostrarMatriz(M).

%! mostrarMatriz(+M)
%
% Muestra una matriz. Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarMatriz(M) :-
	M = [F|_], length(F, Cols),
	mostrarBorde('╔',Cols,'╗'),
	maplist(mostrarFila, M),
	mostrarBorde('╚',Cols,'╝').

mostrarBorde(I,N,F) :-
	write(I),
	stringRepeat('══', N, S),
	write(S),
	write(F),
	nl.

stringRepeat(_, 0, '').
stringRepeat(Str, N, R) :- N > 0, Nm1 is N - 1, stringRepeat(Str, Nm1, Rm1), string_concat(Str, Rm1, R).

%! mostrarFila(+M)
%
% Muestra una lista (fila o columna). Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarFila(Fila) :-
	write('║'),
	maplist(mostrarCelda, Fila),
	write('║'),
	nl.

mostrarCelda(C) :- nonvar(C), C = x, write('██').
mostrarCelda(C) :- nonvar(C), C = o, write('░░').
mostrarCelda(C) :- var(C), write('¿?').
