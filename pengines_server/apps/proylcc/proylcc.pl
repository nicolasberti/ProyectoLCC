:- module(proylcc, 
	[  
		flick/4,
		cantidadAdyacentes/3
	]).

:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 

flick(Grid, Color, FGrid, (C, I, J)):-
	Color \= C,
    pintarAdyacentes(Grid, Color, (C, I, J), FGrid).

% En este codigo no se usa a 0 como el primer elemento, si no a 1.
% Esto por convención en prolog ya que el primer elemento de una lista es el indice 1 y no 0.

/*
 * Funciones principales
 * pintarAdyacentes: para pintar apartir de una cierta celda todos los adyacentes a esa celda.
 * contarAdyacentes: para contar apartir de una cierta celda todos los adyacentes a esa celda.
 * 
 */

% Hay que documentar bien porque decir "devuelve" no es correcto en prolog ya que son predicados
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
        
adyacente(L, (C1,I1,J1), (C1,I2,J2)):-
	member((C1,I1,J1), L),
	member((C1,I2,J2), L),
	(
    	(I1#=I2, J1 #= J2-1);
    	(I1#=I2, J1 #= J2+1);
    	(J1#=J2, I1 #= I2-1);
    	(J1#=J2, I1 #= I2+1) 
    ).
    
adyacenteC(L, X, Y):-
    camino(L, X, Y, _C).
	/*
	 * Una celda X es adyacente transitivamente a una celda Y si y solo si existe un camino
	 * desde X a Y. (L es la lista que contiene todas las celdas)
	 */

camino(L, A,Z,C) :-
	camino_aux(L, A,[Z],C).

camino_aux(_L, A,[A|C1],[A|C1]).
	camino_aux(L, A,[Y|C1],C) :-
	adyacente(L, X,Y),
	not(member(X,[Y|C1])),
	camino_aux(L, A,[X,Y|C1],C).

cantidadAdyacentes(M, (C,I,J), N):-
    adyacentes(M, (C,I,J), Ln),
    length(Ln, N).

adyacentes(M, (C,I,J), Ln):-
	findall( (Cx,Ix,Jx), miembro(M, (Cx,Ix,Jx)), Nodos),
	findall(X, adyacenteC(Nodos, (C,I,J), X), Laux),
	sort(Laux, Ln).