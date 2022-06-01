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



% Estrategia pedida.

buscarSecuencia(M, (C,I,J), N, L, NAdy):-
    buscarSugerencias(M, (C,I,J), N, Ln),
    cantidadAdyacentes(M, (C,I,J), NAdyAux),
    mejorSecuencia(Ln, LAux),
	last(LAux, X),
    NAdy #= X - NAdyAux,
	delete(LAux, X, L),
    retractall(ganoJuego).

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
    last(L1, X),
    last(L2, Y),
    X >= Y,
    !,
    mayorListaAux(L, L1, Zs).
mayorListaAux(L, L1, [L2 | Zs]):-
    last(L1, X),
    last(L2, Y),
    X < Y,
    !,
    mayorListaAux(L, L2, Zs).

% Código base para encontrar secuencias de longitud N (o menor si gana el juego)
buscarSugerencias(M, (C,I,J), N, Ln):-
	findall(X, sugerenciaN(M, (C,I,J), N, X), Ln).

sugerenciaN(M, (C,I,J), 0, [Y]):-
    cantidadAdyacentes(M, (C,I,J), Y).
sugerenciaN(M, (C,I,J), N, [Y]):-
    N>0, gano(M), assert(ganoJuego), 
    cantidadAdyacentes(M, (C,I,J), Y), !.
sugerenciaN(M, (C,I,J), N, [X | Ln]):-
    N>0, not(gano(M)), not(ganoJuego), !, % Agregar not(ganoJuego) mejora la eficiencia cortando cuando encontró una secuencia que gana el juego.
    ( X=r; X=g; X=y; X=b; X=p; X=v ),
    N1 #= N-1,
    flick(M, X, Mn, (C,I,J)),
    sugerenciaN(Mn, (X,I,J), N1, Ln).