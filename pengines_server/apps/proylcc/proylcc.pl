:- module(proylcc, 
	[  
		flick/3
	]).
:- use_module(library(clpfd)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla. 

flick(Grid, Color, FGrid, (C, I, J)):- /*tambien deberia mandar como parametro la celda origen elegida (?)*/
	Color \= X,
	pintarAdyacentes(Grid, Color, (C, I, J), FGrid).

% obtenerAdyacentes(+CeldaI, +CeldaJ, -ListaDeAdyacentesDeEsaCelda)
obtenerAdyacentes(CI, CJ, L):-
	celdaArriba(CI, CJ, LU),
	celdaAbajo(CI, CJ, LD),
	celdaDerecha(CI, CJ, LR),
	celdaIzquierda(CI, CJ, LL),
	L = [LU | LD | LR | LI].

celdaArriba(CI, CJ, LU):-
	CJ > 1,
	LU = [CI, (CJ - 1)].

celdaIzquierda(CI, CJ, LL):-
	CI > 1,
	LL = [(CI - 1), CJ].

celdaAbajo(CI, CJ, LD):-
	CJ < 14,
	LD = [CI, (CJ + 1)].

celdaDerecha(CI, CJ, LR):-
	CI < 14,
	LR = [(CI + 1), CJ].

% Hay que documentar bien porque decir "devuelve" no es correcto en prolog ya que son predicados

% Es verdadero cuando una celda de cierto color C, con una posicion I, J. pertenece a una matriz M 
miembro(M, (C,I,J)):- 
   nth1(I, M, LFila), /*indice, lista, elemento(verdadero cuando el Elem es el elemento de índice de List. El conteo comienza en 1) */
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
	findall( (Cx,Ix,Jx), miembro(M, (Cx,Ix,Jx)), Nodos), //Agrega todas las celdas y las mapea a nodos//
	findall(X, adyacenteC(Nodos, (C,I,J), X), Laux),
	sort(Laux, Ln).

   
        