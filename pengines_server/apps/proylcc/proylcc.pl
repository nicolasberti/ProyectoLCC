:- module(proylcc, 
	[  
		flick/4,
		cantidadAdyacentes/3,
		gano/1,
        sugerirNVeces/5,
		buscarSecuencia/5
	]).

:- use_module(library(clpfd)).

:- dynamic visitado/1. % true sssi un nodo fue visitado.
:- dynamic esAdy/1. % true sssi un nodo es adyacente transitivo de una celda origen.
:- dynamic adyacentesAPintar/1.
:- dynamic ganoJuego/0.
:- dynamic colorAdy/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 

flick(Grid, Color, FGrid, (C, I, J)):-
	Color \= C,
    pintarAdyacentes(Grid, Color, (C, I, J), FGrid), !.

% Flick auxiliar para calcular las secuencias de manera eficiente.
flickAux(Grid, Color, FGrid, (C, I, J), Long):-
	Color \= C,
    pintarAdyacentesAux(Grid, Color, (C, I, J), FGrid, Long), !.


% En este codigo no se usa a 0 como el primer elemento, si no a 1.
% Esto por convención en prolog ya que el primer elemento de una lista es el indice 1 y no 0.

% miembro(+M, +(C,I,J)) true sssi la celda (C,I,J) pertenece a la matriz M
miembro(M, (C,I,J)):-
   nth1(I, M, LFila),
   nth1(J, LFila, C).

% Pinta todos los adyacentes apartir de una celda (C,I,J) de una matriz M por el color Cn
% Devuelve Mn siendo la matriz pintada.
% pintarAdyacentes(+M, +Cn, +(C,I,J), -Mn)
pintarAdyacentes(M, Cn, (C,I,J), Mn):-
    adyacentes(M, (C,I,J), Ln),
    pintar(M, Cn, Ln, Mn).

% Pintar adyacentes auxiliar para calcular las secuencias de manera eficiente.
pintarAdyacentesAux(M, Cn, (C,I,J), Mn, Long):-
    adyacentesNuevo(M, (C,I,J), Ln),
    pintar(M, Cn, Ln, Mn),
    adyacentesNuevo(Mn, (C,I,J), Ln2), % Si bien se calculan 2 veces los adyacentes, no hace ineficiente al algoritmo.
    length(Ln2, Long).

% Pinta todos los adyacentes contenidos en una lista L = [(C,I,J) | Ls] en la matriz M del color Cn
% Mn es la lista con los colores pintados
% pintar(+M, +Cn, +L, -Mn) 
pintar(M, _Cn, [], M).
pintar(M, Cn, [(C,I,J) | Ls], Mn):-
    nth1(I, M, LFila),
    nth1(J, LFila, C),
	replace(C, J, Cn, LFila, LFilaN),
	replace(LFila, I, LFilaN, M, Maux),
	pintar(Maux, Cn, Ls, Mn).

% Codigo re-utilizado
% replace(?X, +XIndex, +Y, +Xs, -XsY)
replace(X, 1, Y, [X|Xs], [Y|Xs]).
replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 1,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

% Apartir de una celda origen, se obtienen todos los adyacentesC
% Los marca dinamicamente a los adyacentes con el hecho esAdy(Celda)
% adyacentesTransitivos(+L, +(C,I,J). L es la lista de celdas que tienen el mismo color que la celda origen
adyacentesTransitivos(L, (C,I,J)):-
    assert( visitado((C,I,J)) );
    ( 
    	IAb #= I+1,
    	member((C,IAb,J), L), 
    	not(visitado((C,IAb,J))), % Si es miembro con ese color, ya es adyacente
			assert(esAdy((C,IAb,J))),
			adyacentesTransitivos(L, (C,IAb,J)) % Da siempre true pero no puede marcar ninguno.
	);
    ( 
    	IAr #= I-1,
    	member((C,IAr,J), L), 
    	not(visitado((C,IAr,J))), % Si es miembro con ese color, ya es adyacente
			assert(esAdy((C,IAr,J))),
			adyacentesTransitivos(L, (C,IAr,J)) % Da siempre true pero no puede marcar ninguno.
	);
    ( 
    	JDer #= J+1,
    	member((C,I,JDer), L), 
    	not(visitado((C,I,JDer))), % Si es miembro con ese color, ya es adyacente
			assert(esAdy((C,I,JDer))),
			adyacentesTransitivos(L, (C,I,JDer)) % Da siempre true pero no puede marcar ninguno.
	);
	( 
    	JIzq #= J-1,
    	member((C,I,JIzq), L), 
    	not(visitado((C,I,JIzq))), % Si es miembro con ese color, ya es adyacente
			assert(esAdy((C,I,JIzq))),
			adyacentesTransitivos(L, (C,I,JIzq)) % Da siempre true pero no puede marcar ninguno.
	).

% Cantidad de adyacentes que contiene la matriz M apartir de una celda origen (C,I,J)
% cantidadAdyacentes(+M, +(C,I,J), -N)
cantidadAdyacentes(M, (C,I,J), N):-
    adyacentes(M, (C,I,J), Ln),
    length(Ln, N).

% Verifica si la matriz tiene todos los colores iguales (sirve para el metodo gano)
iguales([X|Xs]):-
    igualesAux([X|Xs], X).

igualesAux([], _).
igualesAux([X | Xs], Elem):-
       Elem = X,
    igualesAux(Xs, Elem).

% true sssi el juego fue completado.
gano(M):-
    M = [X|Xs],
    iguales(X),
    igualesAux([X|Xs], X). 

% Encuentra todos los adyacentes de la matriz M apartir de una celda origen (C,I,J) y devuelve los adyacentes en Ln
% adyacentes(+M, +(C,I,J), -Ln)
adyacentes(M, (C,I,J), Ln):-
	findall( (C,Ix,Jx), miembro(M, (C,Ix,Jx)), Nodos),
	findall(_, adyacentesTransitivos(Nodos, (C,I,J)), _), 
    	/*
    	 * Como en la consulta adyacentes transitivos hay varios ors, se desea todas las soluciones
    	 * es decir, todas las veces que se cumple el or
    	 */
    assert( esAdy((C,I,J))), % Agrega la celda origen a los adyacentes. (Si no, nunca es marcada)
    findall(X, esAdy(X), Ln), 
    retractall(esAdy(_)), 
    retractall(visitado(_)).	

% Estrategia local.
sugerirNVeces(M, (C,I,J), N, Ln, NAdy):-
    cantidadAdyacentes(M, (C,I,J), NAdy1),
    sugerirNVecesAux(M, (C,I,J), N, Ln, NAdy2),
    NAdy is (NAdy2 - NAdy1). 

% Habria que ver si hay mas casos bases en los cuales se termine la ejecucion del sugerir.
sugerirNVecesAux(M, (C,I,J), 0, [], NAdy2):-
    cantidadAdyacentes(M, (C,I,J), NAdy2).

% A partir de la matriz M, la celda origen, y una cantidad N de sugerencias. Ln es una lista con N sugerencias de colores
% sugerirNVecesAux(+M, +(C,I,J), +N, -Ln)
sugerirNVecesAux(M, (C,I,J), N, [X | Ln], NAdy2):-
	sugerir(M, (C, I, J), X), % me devuelve un color X a pintar que seria el de mayor long de colores
	flick(M, X, Mn, (C, I, J)), % si el color X es el mismo al color C, entonces no sugiere mas colores, por lo tanto termina de realizar las iteraciones (creo)
	Ni is N - 1,
	sugerirNVecesAux(Mn, (X,I,J), Ni, Ln, NAdy2), !. % Agrego ! a lo ultimo para q considere la unica rama

% Predicado de corte si N es mayor q la cantidad de secuencia de colores
sugerirNVecesAux(M, (C,I,J), _, [], NAdy2):-
	gano(M), cantidadAdyacentes(M, (C,I,J), NAdy2).

sugerir(M, (C,I,J), X):-
    adyacentes(M, (C,I,J), L),
    listaBorde(M, L, [], LBorde),
    marcarAdyacente(LBorde),
    buscarAdyacentes(M, LBorde),
    findall( (I1,J1), adyacentesAPintar((r,I1,J1)), LRojo),
    findall( (I2,J2), adyacentesAPintar((g,I2,J2)), LVerde),
    findall( (I3,J3), adyacentesAPintar((b,I3,J3)), LAzul),
    findall( (I4,J4), adyacentesAPintar((y,I4,J4)), LAmarillo),
    findall( (I5,J5), adyacentesAPintar((v,I5,J5)), LVioleta),
    findall( (I6,J6), adyacentesAPintar((p,I6,J6)), LRosa),
    retractall(adyacentesAPintar(_)), 
    length(LRojo,NRojo),
    length(LVerde,NVerde),
    length(LAzul,NAzul),
    length(LAmarillo,NAmarillo),
    length(LVioleta,NVioleta),
    length(LRosa,NRosa),
    not( (length(LRojo, 0), length(LVerde, 0), length(LAzul, 0), length(LAmarillo, 0), length(LVioleta, 0), length(LRosa, 0))),
    LN = [ (r,NRojo), (g,NVerde), (b,NAzul), (y, NAmarillo), (v, NVioleta), (p, NRosa) ],
    colorMayor((X,_), LN), !.
    

buscarAdyacentes(_, []):- !.
buscarAdyacentes(M, [(C,I,J) | Ls]):-
    adyacentes(M, (C,I,J), L),
    marcarAdyacente(L),
    buscarAdyacentes(M, Ls).
    
    
ady( (I1,J1), (I2,J2) ):-
    ( I1 #= I2+1, J1#=J2 );
    ( I1 #= I2-1, J1#=J2 );
    ( J1 #= J2+1, I1#=I2 );
    ( J1 #= J2-1, I1#=I2 ).
    
adyDistintoColor(M, (C1,I1,J1), Ln):-
   	findall((C2,I2,J2), 
            (
              ady( (I1,J1), (I2,J2)),
              miembro(M, (C2,I2,J2)),
              C2\=C1 % No considera los adyacentes del mismo color.
            ), Ln).

listaBorde(_, [], Actuales, Actuales).
listaBorde(M, [(C1,I1,J1) | Ls], Actuales, Nuevos):-
    adyDistintoColor(M, (C1,I1,J1), Ln),
    append(Actuales, Ln, ActualesLn),
    listaBorde(M, Ls, ActualesLn, Nuevos), !.


marcarAdyacente([]):- !.
marcarAdyacente([(C,I,J) | Ls]):-
    not( adyacentesAPintar((C,I,J)) ),
    assert(adyacentesAPintar((C,I,J))),
    marcarAdyacente(Ls).
marcarAdyacente([(_,_,_) | Ls]):- % En caso de que esté repetido, no lo marca.
    marcarAdyacente(Ls).


colorMayor((C, M), [(C1, X)|Xs]):-
          colorMayor((C,M), (C1,X), Xs).
colorMayor((C,M), (C,M), []):- !.
colorMayor((C,X), (_,Y), [(C2,Z)|Zs]):-
          Z >= Y,
          !,
          colorMayor((C,X), (C2,Z), Zs).
colorMayor((C,X), (C1,Y), [(_,Z)|Zs]):-
          Z =< Y,
          colorMayor((C,X), (C1,Y), Zs).


% Estrategia pedida. 5^n

buscarSecuencia(M, (C,I,J), N, L, NAdy):-
    buscarSugerencias(M, (C,I,J), N, Ln),
    cantidadAdyacentes(M, (C,I,J), NAdyAux),
    mejorSecuencia(Ln, LAux),
	maplist(mapear, LAux, L), % LAux es una lista del estilo [(C1,N1), (C2,N2), ..., (Cn, Nn)]. Cn es el color de la secuencia, y Nn es el número de ady capturados.
	last(LAux, (_,X)),
    NAdy #= X - NAdyAux,
    retractall(ganoJuego).

% Devuelve el primer elemento de un par. X=(Y,Z).
mapear((Y,_), Y).

% Busca cual es la mejor secuencia
% hay 2 alternativas
% 1. Ganó el juego, se debe encontrar a la lista con menor length en Ln
% 2. No ganó el juego, se debe encontrar a la lista que tenga el mayor N (cantidad de adyacentes)
mejorSecuencia(Ln, L):-
    ganoJuego, !,
    menorLista(Ln, L).
mejorSecuencia(Ln, L):-
    not(ganoJuego), !, 
    mayorLista(Ln, L).

menorLista([L1 | Xs], L):-
    menorListaAux(L, L1, Xs).
menorListaAux(L, L, []):- !.
menorListaAux(L, L1, [L2 | Zs]):-
    length(L1, X),
    length(L2, Y),
    X =< Y,
    !,
    menorListaAux(L, L1, Zs).
menorListaAux(L, L1, [L2 | Zs]):-
    length(L1, X),
    length(L2, Y),
    X > Y,
    !,
    menorListaAux(L, L2, Zs).

mayorLista([L1 | Xs], L):-
    mayorListaAux(L, L1, Xs).
mayorListaAux(L, L, []):- !.
mayorListaAux(L, L1, [L2 | Zs]):-
    last(L1, (_,X) ),
    last(L2, (_,Y) ),
    X >= Y,
    !,
    mayorListaAux(L, L1, Zs).
mayorListaAux(L, L1, [L2 | Zs]):-
    last(L1, (_,X) ),
    last(L2, (_,Y) ),
    X < Y,
    !,
    mayorListaAux(L, L2, Zs).

% Código base para encontrar secuencias de longitud N (o menor si gana el juego)
buscarSugerencias(M, (C,I,J), N, Ln):-
	findall(X, sugerenciaN(M, (C,I,J), N, X), Ln).

sugerenciaN(_, _, 0, []).
sugerenciaN(M, _, N, []):-
    N>0, gano(M), assert(ganoJuego), !.
sugerenciaN(M, (C,I,J), N, [(X,Long) | Ln]):- % Los elementos de la secuencia tienen el formato (C,N) donde C es el color N los adyacentes capturados (es decir, cantidadAdyacentes).
    N>0, not(gano(M)), not(ganoJuego), !, % Agregar not(ganoJuego) mejora la eficiencia cortando cuando encontró una secuencia que gana el juego.
    ( X=r; X=g; X=y; X=b; X=p; X=v ),
    N1 #= N-1,
    flickAux(M, X, Mn, (C,I,J), Long),
    sugerenciaN(Mn, (X,I,J), N1, Ln).

/*
 * ############### Algoritmo de la catedra ###############
 * 
 * Mas allá de que nuestro algoritmo es eficiente y adopta la misma estrategia,
 * con algoritmo brindado por la catedra, el sugerenciaN se hace mucho más eficiente.
 * 
 * #######################################################
 */

adyacentesNuevo(M, (C,I1,J1), Ln):-
    I is I1-1, % Nosotros usamos el formato [1,1] a [14,14] -> La catedra usa [0,0] a [13,13]
    J is J1-1,
    adyCStar(C, [I,J], M, Ln).

adyCStar(Color, Origin, Grid, Ln) :-
    adyCStarSpread([Origin], [], Grid, Res),
    assert( colorAdy(Color) ),
    maplist(convertirCeldas, Res, LnAux), % La catedra usa el formato [I,J] nosotros usamos (C,I,J)
    maplist(sucesor, LnAux, Ln), % Mapeo el sucesor de I,J por la razón mencionada en adyacentesNuevo.
    retractall(colorAdy(_)).

% Funciones mapeadoras
convertirCeldas(X, Y):-
    X= [I,J],
    colorAdy(Color),
    Y = (Color, I, J).
sucesor( (C,X,Y), (C,I,J) ):-
    I is X+1,
    J is Y+1.
%-------

adyCStarSpread([], Vis, _Grid, Vis).
adyCStarSpread(Pend, Vis, Grid, Res):-
    Pend = [P|Ps],
    findall(A, 
	        (
    	        adyC(P, Grid, A),
        	    not(member(A, Pend)),
            	not(member(A, Vis))
	        ), 
            AdyCP),
    append(AdyCP, Ps, NPend),
    adyCStarSpread(NPend, [P|Vis], Grid, Res).
adyC(P, Grid, A):-
    ady(P, Grid, A),
    color(P, Grid, C),
    color(A, Grid, C).
ady([X, Y], Grid, [X1, Y]):-
    length(Grid, L),
    X < L - 1,
    X1 is X + 1.
ady([X, Y], _Grid, [X1, Y]):-
    X > 0,
    X1 is X - 1.
ady([X, Y], Grid, [X, Y1]):-
    Grid = [F|_],
    length(F, L),
    Y < L - 1,
    Y1 is Y + 1.
ady([X, Y], _Grid, [X, Y1]):-
    Y > 0,
    Y1 is Y - 1.
color([X,Y], Grid, C):-
    nth0(X, Grid, F),
    nth0(Y, F, C). 