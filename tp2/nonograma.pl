

% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 1
%
%! matriz(+F, +C, -M)
matriz(F, C, M) :-
	length(M, F),
	nth1(1, M, FirstRow),
	length(FirstRow, C),
	maplist(same_length(FirstRow), M).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 2
%
%! replicar(?E, ?N, ?L)
replicar(E, N, L) :-
	length(L, N),
	maplist(=(E), L).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 3
%
%! transponer(+M, -MT)
transponer(M, MT) :-
	matriz(_, C, M),
	matriz(C, 0, MT0),
	foldl(agregar_a_columna, M, MT0, MT).

% AUXILIAR
%
%! agregar_a_columna(+FilaOriginal, +TranspuestaVacia, -MatrizTranspuesta)
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
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 4
%
%! pintadasValidas(+R)
pintadasValidas(r(Restric, Linea)) :-
	length(Linea, LTotal),
	length(Restric, LRestric),
	sum_list(Restric, CPintada),
	MaxEspacio is max(LTotal - CPintada - (LRestric - 1) + 1, 0),
	% generate
	generarPosibles(Restric, LTotal, MaxEspacio, LineaPosible),
	agregarEspaciosBorde(LTotal, LineaPosible, Posible),
	% test (verificamos la longitud y la unificación chequea variables con valor y el resto)
	same_length(Linea, Posible),
	Posible = Linea.

% AUXILIAR
%
%! generarPosibles(+Restric, +LongTotal, +MaxEspacio, -LineaPosible)
generarPosibles([], LTotal, _, LineaPosible) :-
	replicar(o, LTotal, LineaPosible).
generarPosibles([N], _, _, LineaPosible) :-
	replicar(x, N, LineaPosible).
generarPosibles( [N | T], LTotal, MaxEspacio, LineaPosible) :-
	generarPosibles(T, LTotal, MaxEspacio, LineaTail),
	agregarEspacioInterno(MaxEspacio, LineaTail, LineaTailConEspacio),
	replicar(x, N, Pintada),
	append(Pintada, LineaTailConEspacio, LineaPosible),
	length(LineaPosible, LLineaPosible),
	LLineaPosible =< LTotal.

% AUXILIAR
%
%! agregarEspacioInterno(+MaxEspacio, +Tail, +TailConEspacios)
agregarEspacioInterno(MaxEspacio, LineaTail, LineaTailConEspacio) :-
	between(1, MaxEspacio, Len),
	replicar(o, Len, Espacios),
	append(Espacios, LineaTail, LineaTailConEspacio).

% AUXILIAR
%
%! agregarEspaciosBorde(+LTotal, +LineaPosible, -Posible)
agregarEspaciosBorde(LTotal, LineaPosible, Posible) :-
	length(LineaPosible, CurLen),
	CantRelleno is max(LTotal - CurLen, 0),
	replicar(o, CantRelleno, Relleno),
	append(Prefix, Postfix, Relleno),
	append3(Prefix, LineaPosible, Postfix, Posible).

% AUXILIAR
%
%! append3(+Prefix, +Lista, +Postfix, -Resultado)
append3(Prefix, Lista, Postfix, Resultado) :-
	append(Prefix, Lista, ParteIzq),
	append(ParteIzq, Postfix, Resultado).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 5
%
%! resolverNaive(+NN)
resolverNaive(nono(_, RS)) :-
	maplist(pintadasValidas, RS).



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 6
%
% Por simplicidad, se mira la cardinalidad del conjunto de valores que puede tomar una
% columna (si es único entonces es una pintada obligatoria). Dado que las pintadas
% obligatorias se calculan por fila de la matriz (y todas las posibles pintadas), usamos
% la transpuesta para separar los posibles valores POR columna.
% Por este motivo no vimos necesidad de utilizar el predicado combinarCelda provisto.
%
%! pintarObligatorias(+R)
pintarObligatorias(r(Restric, Linea)) :-
	findall(Linea, pintadasValidas(r(Restric, Linea)), PosiblesLineas),
	transponer(PosiblesLineas, Lineas),
	celdasObligatorias(Lineas, Linea).

% AUXILIAR
%
%! celdasObligatorias(+Lineas, +Linea)
celdasObligatorias([], []).
celdasObligatorias([HLs | TLs], [HL | TL]) :-
	celdasObligatorias(TLs, TL),
	list_to_set(HLs, Valores),
	reducirValorObligatorio(Valores, HL).

% AUXILIAR
%
% Si el conjunto tiene un único valor, entonces el valor es obligatorio.
%
%! reducirValorObligatorio(+Valores, +Variable)
reducirValorObligatorio([], _).
reducirValorObligatorio([Valor], Valor).
reducirValorObligatorio([_, _ | _], _).


% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 7
%
%! deducir1Pasada(+NN)
deducir1Pasada(nono(_, RS)) :-
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
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 8
%
%! restriccionConMenosLibres(+NN, -R)
restriccionConMenosLibres(nono(_, RS), R) :-
	unaRestriccion(RS, R, FV),
	not((unaRestriccion(RS, _, NFV), FV > NFV)).

%! unaRestriccion(+RS, -R, -FV)
unaRestriccion(RS, R, FV) :-
	member(R, RS),
	R = r(_, L),
	cantidadVariablesLibres(L, FV), FV > 0.



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 9
%
% Intenta resolver recursivamente, mientras queden celdas sin decidir. Para esto
% usa la cantidad de celdas no asignadas (variables libres) e intenta mientras
% la cantidad vaya decreciendo hasta cero (ya resuelto).
%
%! resolverDeduciendo(+NN)
resolverDeduciendo(NN) :-
	NN = nono(M,_),
	deducirVariasPasadas(NN),
	cantidadVariablesLibres(M, FV),
	resolverDeduciendoCont(NN, FV).

% AUXILIAR
%
%! resolverDeduciendoCont(+NN, +FV)
resolverDeduciendoCont(_, 0). 							% no hay variables libres => solución!
resolverDeduciendoCont(NN, FV) :-
	FV > 0, 											% aun no resuelto
	restriccionConMenosLibres(NN, r(Restric, Linea)),
	%
	% se hace un CUT porque cualquiera sea la restricción mínima, si el nonograma
	% tiene solución, por esa restricción debería llegar a la respuesta. Intentar
	% con otras alternativas duplicaría los resultados al encontrarlos en otra
	% rama del árbol de búsqueda.
	%
	!,													
	findall(Linea, pintadasValidas(r(Restric, Linea)), PosiblesLineas), % buscamos las pintadas validas y probamos
																		% una por una intentado resolver
	member(Linea, PosiblesLineas),
	resolverDeduciendo(NN).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 10
%
% IMPORTANTE: si resolverDeduciendo debido a un error retorna soluciones duplicadas,
% bagof garantiza que se consideran soluciones únicas para decidir.
%
%! solucionUnica(+NN)
solucionUnica(nono(M, R)) :-
	bagof(M, resolverDeduciendo(nono(M, R)), Soluciones),
	length(Soluciones, 1).



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 11 - INICIO
%
% intento de automatizar el análisis
%
% IMPORTANTE: decidimos implementar esta automatización para no tener que correr
% a mano mútiples veces las mismas consultas. Cumple las indicaciones del TP.

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
	include(esNonoPredefinido, Numeros, Predefinidos),
	exclude(esNonoPredefinido, Numeros, NoPredefinidos),
    maplist(realizaAnalisis, Predefinidos, PredefInfo),
    maplist(realizaAnalisis, NoPredefinidos, NoPredefInfo),
	mostrarInformacion(PredefInfo, NoPredefInfo).

%! esNonoPredefinido(+N)
esNonoPredefinido(N) :- N < 100.

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
	successOrFailure(solucionUnica(NN), SolucionUnica),
	successOrFailure(resuelveSinBacktraking(NN), SinBacktracking).

%
% IDEA: Obtiene el tamaño de un nonograma. Es diferente al provisto en el enunciado porque
% aquí ya tenemos el nonograma y es más facil usarlo que crearlo nuevamente.
%
%! tamaño(+NN, -F, -C)
tamaño(nono(M, _), F, C) :- matriz(F, C, M).

%
% IDEA: Intenta resolver el nonograma haciendo múltiples pasadas (es decir, intenta pintar
% obligatorias solamente). Si al terminar las pasadas, no hay variables libres, entonces puede
% resolverse sin backtracking. Notar que las pasadas buscando celdas obligatorias termina en
% uno de dos escenarios:
% 1: si se pudo resolver (no hay variables libres), tiene solución UNICA.
% 2: si no se pudo resolver (hay variables libres), sí sabemos que todo lo fijo está definido
%    y el resto está libre.
% En cualquiera de los dos casos, sólo hay un posible resultado, por eso no usaremos metapredicados
%
%! resuelveSinBacktracking(+NN)
resuelveSinBacktraking(nono(M, R)) :-
    deducirVariasPasadas(nono(M, R)),
	cantidadVariablesLibres(M, FV),
	FV =:= 0.

%
% IDEA: Si puede cumplir el objetivo, entonces retorna éxito (cumple el objetivo y cut, para que no
% siga). Si no pudo, entonces indica falla.
%
%! successOrFailure(+Goal, -Result)
successOrFailure(Goal, succeeded) :- Goal, !.
successOrFailure(_, failed).

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
mostrarInformacion(PredefInfo, NoPredefInfo) :-
	write('--'), nl,
	write('-- Esto es el output del predicado ejercicio11() que se puede utilizar en la consola'), nl,
	write('-- luego de cargar el archivo nonograma.'), nl,
	write('--'), nl,
	write('--'), nl,
	mostrarInfoBorde('┌', '┐', '┬'),
	mostrarFila('N', 'Tamaño', '¿Tiene solución única?', '¿Es deducible Sin backtracking?'),
	mostrarInfoBorde('├', '┤', '┼'),
	maplist(mostrarInfo, PredefInfo),
	mostrarInfoBorde('├', '┤', '┼'),
	maplist(mostrarInfo, NoPredefInfo),
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

% EJEMPLOS DE DISCORD
nn(100, NN) :- armarNono([[4],[3],[2,1],[1,3],[1,1],[3],[2]], [[4],[3],[2,1],[1,3],[1,1],[3],[2]], NN).
nn(101, NN) :- armarNono([[2],[4,1],[1,1],[2,1,2],[9],[7,1],[9],[6,2],[4,2],[5]], [[1],[1,4],[2,6],[2,7],[1,6],[8],[1,4,1],[4,2],[2,3],[4]], NN).

% EJEMPLOS DE NONOGRAMS.ORG
nn(200, NN) :- armarNono([[1],[2],[3],[1,3],[3,1],[2,5],[1,2,1],[1,1,7],[2,2,1],[2,1,8],[1,1,2,1],[1,1,1,9],[2,1,2,1],[17],[1],[20],[1,1,1,1,1,1,1],[2,2,2,2,2,3],[3,2],[12]],[[1],[2],[1,1,1],[1,1,2],[3,2,1],[2,2,1,3],[3,1,1,1,1,1],[2,1,1,2,2,1],[3,1,1,1,1,1,1,1],[2,1,1,1,1,2,1,1,1],[17,1],[2,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1],[2,1,1,1,1,2,1],[1,1,1,1,1,1,1,1],[2,1,1,1,1,1,1],[3,1,1,2,2],[3,1,1,2],[3,1,1],[3]],NN).

% EJEMPLO DE DISCORD

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
