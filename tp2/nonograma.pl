

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
pintadasValidas(R) :-
	R = r(Restric, Linea),
	length(Linea, LTotal),
	length(Restric, LRestric),
	suma(Restric, CPintada),
	CLibre is LTotal - CPintada,
	CLibre >= 0,
	espaciosMinimos(LRestric, EspaciosMinimos),
	generarPosibles(EspaciosMinimos, Restric, CLibre, LTotal, Posible),
	Posible = Linea.

% AUXILIAR - sumatoria de elementos (ENTEROS) de una lista
%! suma(+L, ?Total)
suma([], 0).
suma([ Head | Tail ], Total) :- suma(Tail, Subtotal), Total is Subtotal + Head.

% AUXILIAR - arma una lista conteniendo el punto de inicio para cada espacio
% (0 para inicial/final y 1 para el resto)
%! espaciosMinimos(+CantRestric, -Espacios)
espaciosMinimos(0, [ 0 ]).
espaciosMinimos(LRestrict, Espacios) :-
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
resolverNaive(NN) :-
	NN = nono(_, RS),
	maplist(pintadasValidas, RS).



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 6
%
% IDEA: Lo que hacemos es calcular todas las pintadas válidas para la restriccion. Esto
% retorna genera una lista donde cada fila es una pintada válida, de forma que las columnas
% de cada fila corresponden a una celda de la restriccion. Por eso calculamos la transpuesta
% (haciendo que cada fila de la nueva matriz corresponda a los posibles valores para la celda
% especifica). Hecho esto, cada fila de la matriz transpuesta corresponde a una celda de
% la restriccion. Lo que queda es armar un "set" con los valores posibles de cada fila y
% verificar si ese set tiene longitud 0, 1 o 2. Si tiene un único elemento, entonces la
% celda de la restriccion siempre estará pintada con ese elemento (x u o) y se puede
% considerar como una celda obligatoria.
%
%! pintarObligatorias(+R)
pintarObligatorias(R) :-
	R = r(Restric, Linea),
	findall(Linea, pintadasValidas(r(Restric, Linea)), PosiblesLineas),
	transponer(PosiblesLineas, Lineas),
	lineasObligatorias(Lineas, Linea).

% AUXILIAR - recibe una lista de lineas (cada línea representando valores de una columna) y
% arma una lista resultado, donde cada celda se fija a un valor (si la línea tiene el mismo
% valor en todas las celdas), sino deja la celda sin modificar.
%! lineasObligatorias(+Lineas, ?Linea)
lineasObligatorias([], []).
lineasObligatorias([HLs | TLs], [HL | TL]) :-
	lineasObligatorias(TLs, TL),
	list_to_set(HLs, Valores),
	reducirValorObligatorio(Valores, HL).

% AUXILIAR - recibe una lista de valores únicos (que aparecían en las posibles pintadas). Si todos
% son el mismo valor (la lista tiene longitud 1), entonces se fija a ese valor. Sino se deja la variable
% sin modificar.
%! reducirValorObligatorio(+Valores, +Variable)
reducirValorObligatorio([], _).
reducirValorObligatorio([V], HL) :- HL = V.
reducirValorObligatorio([_, _ | _], _).


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
%! deducir1Pasada(+NN)
deducir1Pasada(NN) :-
	NN = nono(_, RS),
	maplist(pintarObligatorias, RS).


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
%! restriccionConMenosLibres(+NN, -R)
restriccionConMenosLibres(nono(_, RS), R) :- unaRestriccion(RS, R, FV), not((unaRestriccion(RS, _, NFV), FV > NFV)).

%! unaRestriccion(+RS, -R, -FV)
unaRestriccion(RS, R, FV) :- member(R, RS), R = r(_, L), cantidadVariablesLibres(L, FV), FV > 0.



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 9
%! resolverDeduciendo(+NN)
resolverDeduciendo(NN) :-
	NN = nono(M,_),
	deducirVariasPasadas(NN),
	cantidadVariablesLibres(M, FV),
	resolverDeduciendoCont(NN, FV).

%! resolverDeduciendoCont(+NN, +FV)
resolverDeduciendoCont(_, 0).
resolverDeduciendoCont(NN, FV) :-
	NN = nono(M,_),
	FV > 0,
	restriccionConMenosLibres(NN, r(Restric, Linea)),
	!,
	findall(Linea, pintadasValidas(r(Restric, Linea)), PosiblesLineas),
	member(LineaPosible, PosiblesLineas),
	Linea = LineaPosible,
	deducirVariasPasadas(NN),
	cantidadVariablesLibres(M, FV1),
	resolverDeduciendoCont(NN, FV1).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 10
%! solucionUnica(+NN)
solucionUnica(NN) :-
	NN = nono(M, R),
	setof(M, resolverDeduciendo(nono(M, R)), Soluciones),
	length(Soluciones, 1).



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 11 - INICIO
%
% intento de automatizar el análisis

%
% Resuelve la tabla del ejercicio 11. Busca primero todos los números de nonograma
% predefinidos y para cada uno realiza el análisis necesario (obtener el tamaño,
% decidir si tiene solucion única y si se puede resolver sin backtracking) y arma
% una lista con esa información.
% Luego, procesa la lista con la información y genera salida por pantalla.
%
%! ejercicio11()
ejercicio11() :-
    findall(Numero, nn(Numero, _), Numeros),
    maplist(realizaAnalisis, Numeros, Informacion),
	mostrarInformacion(Informacion).

%
% Realiza el análisis de un nonograma
%
% IMPORTANTE: 	aqui usamos algo obtenido de internet, que genera un valor (succeeded o failed)
% 				si el predicado tiene éxito o falla (ver check_success_failure más abajo)
%
%! realizaAnalisis(+Nro, -NNI)
realizaAnalisis(Numero, nonoInfo(Numero, (F, C), SolucionUnica, SinBacktracking)) :-
    nn(Numero, NN),
    tamaño(NN, F, C),
	check_success_failure(solucionUnica(NN), SolucionUnica),
	check_success_failure(resuelveSinBacktraking(NN), SinBacktracking).

%
% Obtiene el tamaño de un nonograma. Es diferente al provisto para el ejercicio porque
% usa el predicado matriz como si fuera reversible, pero no lo es
%
%! tamaño(+NN, -F, -C)
tamaño(nono(M, _), F, C) :-
	length(M, F),
	nth1(1, M, PrimeraFila),
	length(PrimeraFila, C).

%
% Intenta resolver el nonograma haciendo múltiples pasadas. Si al terminar las pasadas,
% no hay variables libres, entonces puede resolverse sin backtracking
%
%! resuelveSinBacktracking(NN)
resuelveSinBacktraking(NN) :-
    findall(NN, deducirVariasPasadas(NN), Soluciones),
    maplist(resuelto, Soluciones).

%! resuelto(NN)
resuelto(nono(M, _)) :- cantidadVariablesLibres(M, FV), FV =:= 0.

%
% Buscamos en internet la forma de obtener un booleano para saber si un predicado
% tuvo éxito o falló y obtuvimos este código
%
%! check_success_failure(+Goal, -Result)
check_success_failure(Goal, Result) :-
    (   call(Goal)
    ->  Result = succeeded
    ;   Result = failed
    ).

%
% Convierte succceded o failed a un string (Si, No)
%
%! statusToString(+Status, -String)
statusToString(succeeded, 'Si').
statusToString(failed, 'No').

%
% Genera la tabla que muestra la información recopilada sobre los nonogramas. Esto
% es una especie de clon mutado del predicado provisto mostrarNono.
% Lo que hace es generar la base de la tabla (bordes y títulos) y luego mostrar
% formateada la información obtenida para cada nonograma.
%
%! mostrarInformacion(+Informacion)
mostrarInformacion(Informacion) :-
	mostrarInfoBorde('┌', '┐', '┬'),
	mostrarFila('N', 'Tamaño', '¿Tiene solución única?', '¿Es deducible Sin backtracking?'),
	mostrarInfoBorde('├', '┤', '┼'),
	maplist(mostrarInfo, Informacion),
	mostrarInfoBorde('└', '┘', '┴').

%
% Escribe la información de un nonograma
%
%! mostrarInfo(+NNInfo)
mostrarInfo(nonoInfo(Numero, (F, C), SolucionUnica, SinBacktracking)) :-
	number_string(Numero, SNumero),
	number_string(F, SF),
	number_string(C, SC),
	string_concat(SF, ' x ', STamaño1),
	string_concat(STamaño1, SC, STamaño),
	statusToString(SolucionUnica, SSolucionUnica),
	statusToString(SinBacktracking, SSinBacktracking),
	mostrarFila(SNumero, STamaño, SSolucionUnica, SSinBacktracking).

%
% Escribe una línea de borde (superior, inferior y separador)
%
%! mostrarInfoBorde(+BordeIzq, +BordeDer, +SepColumna)
mostrarInfoBorde(BordeIzq, BordeDer, SepColumna) :-
	stringRepeat('─', 8, S1),
	stringRepeat('─', 12, S2),
	stringRepeat('─', 25, S3),
	stringRepeat('─', 35, S4),
	write(BordeIzq),
	write(S1), write(SepColumna),
	write(S2), write(SepColumna),
	write(S3), write(SepColumna),
	write(S4),
	write(BordeDer),
	nl.

%
% Escribe la información de un nonograma
%
%! mostrarFila(+Numero, +Tamaño, +SolucionUnica, +SinBacktracking)
mostrarFila(Numero, Tamaño, SolucionUnica, SinBacktracking) :-
	stringJustified(Numero,           8, JNumero),
	stringJustified(Tamaño,          12, JTamaño),
	stringJustified(SolucionUnica,   25, JSolucionUnica),
	stringJustified(SinBacktracking, 35, JSinBacktracking),
	write('│'),
	write(JNumero), write('│'),
	write(JTamaño), write('│'),
	write(JSolucionUnica), write('│'),
	write(JSinBacktracking),
	write('│'),
	nl.

%
% Escribe un string justificado según un tamaño
%
%! writeStringFilled(+S, +Size, -JustS)
stringJustified(S, Size, JustS) :-
	string_length(S, SLen),
	Padding is Size - SLen,
    RightPadding is Padding // 2,
    LeftPadding is Padding - RightPadding,
	stringRepeat(' ', LeftPadding, LPadding),
	stringRepeat(' ', RightPadding, RPadding),
	string_concat(LPadding, S, JPartial),
	string_concat(JPartial, RPadding, JustS).

%
% Ejercicio 11 - FIN
%
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------


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
