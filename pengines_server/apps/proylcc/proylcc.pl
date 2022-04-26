:- module(proylcc, 
	[  
		flick/4,
		cantidadAdyacentes/3,
		gano/1
	]).

:- use_module(library(clpfd)).

:- dynamic visitado/1. % true sssi un nodo fue visitado.
:- dynamic esAdy/1. % true sssi un nodo es adyacente transitivo de una celda origen.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 

flick(Grid, Color, FGrid, (C, I, J)):-
	Color \= C,
    pintarAdyacentes(Grid, Color, (C, I, J), FGrid), !.

% En este codigo no se usa a 0 como el primer elemento, si no a 1.
% Esto por convención en prolog ya que el primer elemento de una lista es el indice 1 y no 0.

/*
 * Funciones principales
 * pintarAdyacentes: para pintar apartir de una cierta celda todos los adyacentes a esa celda.
 * contarAdyacentes: para contar apartir de una cierta celda todos los adyacentes a esa celda.
 * 
 */

% Hay que documentar bien porque decir "devuelve" no es correcto en prolog ya que son predicados
% mapea un elemento de la matriz a un vector (C,I,J) (color y posiciones)
miembro(M, (C,I,J)):-
   nth1(I, M, LFila),
   nth1(J, LFila, C).

% Pinta todos los adyacentes apartir de una celda (C,I,J) de una matriz M por el color Cn
% Devuelve Mn siendo la matriz pintada.
pintarAdyacentes(M, Cn, (C,I,J), Mn):-
    adyacentes(M, (C,I,J), Ln),
    pintar(M, Cn, Ln, Mn).

% Pinta todos los adyacentes contenidos en una lista [(C,I,J) | Ls] en la matriz M del color Cn
% Mn es la lista con los colores pintados
pintar(M, _Cn, [], M).
pintar(M, Cn, [(C,I,J) | Ls], Mn):-
    nth1(I, M, LFila),
    nth1(J, LFila, C),
	replace(C, J, Cn, LFila, LFilaN),
	replace(LFila, I, LFilaN, M, Maux),
	pintar(Maux, Cn, Ls, Mn).

%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
replace(X, 1, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 1,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

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

% adyacenteC(X, Y) se verifica si existe un camino desde X a Y
% como siempre se va a verificar que adyacenteC( celdaOrigen, Y)
% y adyacenteC(X, celdaOrigen), entonces se puede afirmar que adyacenteC(X,Y)

cantidadAdyacentes(M, (C,I,J), N):-
    adyacentes(M, (C,I,J), Ln),
    length(Ln, N).

iguales([X|Xs]):-
    igualesAux([X|Xs], X).

igualesAux([], _).
igualesAux([X | Xs], Elem):-
       Elem = X,
    igualesAux(Xs, Elem).

gano(M):-
    M = [X|Xs],
    iguales(X),
    igualesAux([X|Xs], X). 

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