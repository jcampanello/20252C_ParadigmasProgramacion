

% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 1

%! matriz(+F, +C, -M)
matriz(F, C, M) :-
	length(M, F),
	nth1(1, M, FirstRow),
	length(FirstRow, C),
	maplist(same_length(FirstRow), M).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 2

%! replicar(?E, ?N, ?L)
replicar(E, N, L) :- length(L, N), maplist(=(E), L).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 3
%! transponer(+M, -MT)
transponer(M, MT) :-
	matriz(_, C, M),
	matriz(C, 0, MT0),
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
%
% Primero se hacen algunas cuentas, para saber cual es la cantidad de celdas pintadas,
% espacios forzados y rango de espacio libre para asignar.
% Luego se crea una lista con el punto de inicio para los espacios (si hay N restricciones
% se crean N+1 espacios: N-1 entre pintadas y 2 para inicio y fin). El primero y último
% tienen punto de inicio 0 (son opcionales), los intermedios tienen punto de inicio 1
% (se requiere un mínimo de un espacio entre celdas pintadas).
% Luego se realiza un Generate and Test. Generate genera todas las posibles pintadas
% considerando que cada posible espacio va de la cantidad mínima al máximo disponible.
% Esto puede generar líneas posibles de mayor longitud, pero estos casos se eliminan
% en la parte Test (se trata de unificar con la Línea de la restricción). Esta
% unificación verifica la longitud y además 
%
%! pintadasValidas(+R)
pintadasValidas(r(Restric, Linea)) :-
	length(Linea, LTotal),
	length(Restric, LRestric),
	sum_list(Restric, CPintada),
	CLibre is LTotal - CPintada,
	CLibre >= 0,
	espaciosMinimos(LRestric, EspaciosMinimos),
	% generate
	generarPosibles(EspaciosMinimos, Restric, CLibre, LTotal, Posible),
	% test (verificamos la longitud y la unificación chequea variables con valor y el resto)
	same_length(Linea, Posible),
	Posible = Linea.

% AUXILIAR - arma una lista conteniendo el punto de inicio para cada espacio
% (0 para inicial/final y 1 para el resto). Si hay N pintadas (restricciones)
% quedan N+1 espacios.
%
%! espaciosMinimos(+CantRestric, -Espacios)
espaciosMinimos(0, [ 0 ]).
espaciosMinimos(LRestrict, Espacios) :-
	LRestrict > 0,
	Internos is LRestrict - 1,
	replicar(1, Internos, EspaciosInternos),
	append( [ 0 ], EspaciosInternos, EspaciosIniciales),
	append(EspaciosIniciales, [ 0 ], Espacios).

% AUXILIAR - genera una posible combinacion para la linea. Los parámetros del predicado son:
% - lista de puntos de inicio (N+1) para los espacios (0 o 1), que pueden crecre hasta el valor de Rango
% - lista de pintadas (N) con las longitudes a pintar
% - longitud maxima de un segmento de espacios
% - longitud máxima de la línea
% - posible línea
%
% Lo que hace es consumir en cada paso un par de elementos (inicio mínimo del espacio y una pintada) y
% genera recursivamente lo que sigue (consumiendo de a pares) hasta que queda sólo un elemento de inicio
% de espacio (el espacio final). En este caso, sólo genera los posibles espacios.
% En cada paso recursivo entonces toma la cola generada y le agrega la pintada y los posibles espacios
% adelante. En varios puntos se va chequeando longitud total, para reducir esfuerzo de generación
% que se sabe no será válido.
%
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

% AUXILIAR - genera las posibilidades de espacios previos. Toma una cantidad inicial y una cantidad
% máxima y para cada entero dentro de ese rango, genera una lista con esa cantidad de espacios.
% La cantidad inicial puede ser 0 (para el primer y último espacio) o uno, para los espacios
% intermedios.
%
%! espaciosPrevios(+CantInicial, +CantMaxima, ?Espacios)
espaciosPrevios(CantInicial, CantMaxima, Espacios) :-
	between(CantInicial, CantMaxima, Len),
	replicar(o, Len, Espacios).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 5
%! resolverNaive(+NN)
resolverNaive(nono(_, RS)) :-
	maplist(pintadasValidas, RS).



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 6
%
% IDEA: Lo que hacemos es calcular todas las pintadas válidas para la restriccion. Esto
% genera una lista donde cada fila es una pintada válida, de forma que las columnas
% de cada fila corresponden a una celda de la restriccion. Por eso calculamos la transpuesta.
% Hecho esto, cada fila de la matriz transpuesta corresponde a una celda de la restriccion.
% El siguiente paso es armar un "set" con los valores posibles de cada fila y verificar si
% ese set tiene longitud 0, 1 o 2. Si tiene un único elemento, entonces la celda de la
% restriccion siempre estará pintada con ese elemento (x u o) y se puede considerar como
% una celda obligatoria.
%
%! pintarObligatorias(+R)
pintarObligatorias(r(Restric, Linea)) :-
	findall(Linea, pintadasValidas(r(Restric, Linea)), PosiblesLineas),
	transponer(PosiblesLineas, Lineas),
	lineasObligatorias(Lineas, Linea).

% AUXILIAR - recibe una lista de lineas (cada línea representando valores de una columna) y
% arma una lista resultado, donde cada celda se fija a un valor (si la línea tiene el mismo
% valor en todas las celdas), sino deja la celda sin modificar.
%
%! lineasObligatorias(+Lineas, +Linea)
lineasObligatorias([], []).
lineasObligatorias([HLs | TLs], [HL | TL]) :-
	lineasObligatorias(TLs, TL),
	list_to_set(HLs, Valores),
	reducirValorObligatorio(Valores, HL).

% AUXILIAR - toma una lista de valores únicos (que aparecían en las posibles pintadas). Si todos
% son el mismo valor (la lista tiene longitud 1), entonces se fija a ese valor. Sino se deja la
% variable sin modificar. El predicado es completo y nunca falla, solo fija valor cuando
% corresponde.
%
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
%
% IDEA: para deducir una pasada, tratamos de identificar todas las celdas obligatorias
% en cada fila y las pintamos
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
%
% Ejercicio 8
%
% IDEA: se busca una restricción y luego se verifica que no existe otra restricción
% que tenga menos variables libres.
%
%! restriccionConMenosLibres(+NN, -R)
restriccionConMenosLibres(nono(_, RS), R) :- unaRestriccion(RS, R, FV), not((unaRestriccion(RS, _, NFV), FV > NFV)).

%! unaRestriccion(+RS, -R, -FV)
unaRestriccion(RS, R, FV) :- member(R, RS), R = r(_, L), cantidadVariablesLibres(L, FV), FV > 0.



% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 9
%
% IDEA: se realiza un ciclo en donde primero se intenta deducir varias pasadas (es decir:
% identificar y pintar celdas que se sabe son obligatorias). Luego se realiza una "iteración"
% eligiendo una restricción y probando las posibles líneas de la misma, para intentar
% resolver utilizando cada línea posible. Esta iteración ocurre mientras la cantidad de
% variables libres no sea cero.
%
%! resolverDeduciendo(+NN)
resolverDeduciendo(NN) :-
	NN = nono(M,_),
	deducirVariasPasadas(NN),
	cantidadVariablesLibres(M, FV),
	resolverDeduciendoCont(NN, FV).

%! resolverDeduciendoCont(+NN, +FV)
resolverDeduciendoCont(_, 0). 												% no hay variables libres => solución!
resolverDeduciendoCont(NN, FV) :-
	FV > 0, 																% hace disjunto el predicado anterior
	restriccionConMenosLibres(NN, r(Restric, Linea)), 						% buscamos una restricción mínima
	%
	% aquí realizamos un CUT porque puede haber más de una línea (con variables
	% no instanciadas) y con la mínima cantidad de restricciones. La idea del
	% algoritmo es pensar que cualquiera de las restricciones con cantidad mínima
	% es equivalente, porque las otras restricciones serán deducidas de forma
	% simple o serán tratadas más adelante. Si no se realiza el cut, luego de
	% resolver usando la restricción elegida, se usaría otra restricción mínima
	% en el mismo punto, generando otra solución equivalente (pero explorada por
	% otra rama del árbol).
	%
	!,
	findall(Linea, pintadasValidas(r(Restric, Linea)), PosiblesLineas), 	% buscamos todas las líneas posibles
	member(LineaPosible, PosiblesLineas), 									% elegimos las posibles, de a una
	Linea = LineaPosible, 													% unificamos => falla+próxima línea o continúa
	deducirVariasPasadas(NN), 												% trata de deducir (o llegar a una inconsistencia)
	cantidadVariablesLibresNono(NN, FV1), 									% itera
	resolverDeduciendoCont(NN, FV1).

% IDEA: este predicado se creó como un "wrapper" que opera sobre el nonograma (en lugar de sólo
% la matriz, como lo hace cantidadVariablesLibres). Usar este predicado evita la necesidad de
% abrir el nonograma dentro de resolverDeduciendoCont.
%
%! cantidadVariablesLibresNono(+NN, -N)
cantidadVariablesLibresNono(nono(M,_), N) :-
	cantidadVariablesLibres(M, N).


% --------------------------------------------------------------------------------
% --------------------------------------------------------------------------------
%
% Ejercicio 10
%
% IDEA: obtenemos todas las posibles soluciones (únicas) y verificamos que se haya
% obtenido una sola solución.
%
%! solucionUnica(+NN)
solucionUnica(nono(M, R)) :-
	findall(M, resolverDeduciendo(nono(M, R)), Soluciones),
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
	check_success_failure(solucionUnica(NN), SolucionUnica),
	check_success_failure(resuelveSinBacktraking(NN), SinBacktracking).

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
% IDEA: Buscamos en internet la forma de obtener un booleano para saber si un predicado
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
