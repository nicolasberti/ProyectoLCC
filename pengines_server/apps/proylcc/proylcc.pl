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

flick(Grid, Color, FGrid):-
	Grid = [F|Fs],
	F = [X|Xs],
	Color \= X,
	FGrid = [[Color|Xs]|Fs],
	findall( (C, I, J), miembro(FGrid, C, I, J), L),
	findnsols(194, (C1, I1, J1), (adyacenteC(L, (g,2,2), (C1,I1,J1))), Ln),
	sort(Ln, Lnm).
	
miembro(M, C, I, J):-
   nth1(I, M, LFila), //indice, lista, elemento
   nth1(J, LFila, C).

adyacente(L, (C1, I1, J1), (C1, I2, J2) ):-
member((C1, I1, J1), L),
member((C1, I2, J2), L),
( (I1#=I2, J1 #= J2-1);
    (I1#=I2, J1 #= J2+1);
    (J1#=J2, I1 #= I2-1);
    (J1#=J2, I1 #= I2+1) ).


adyacenteC(L, (C1, I1, J1), (C1, I2, J2) ):-
    adyacente(L, (C1, I1, J1), (C1, I2, J2) ).

adyacenteC(L, (C1, I1, J1), (C1, I2, J2) ):-
    adyacente(L, (C1, I1, J1), (C1, I3, J3) ),
    adyacenteC(L, (C1, I3, J3), (C1, I2, J2) ).

camino(L, A,Z,C) :-
camino_aux(L, A,[Z],C).

camino_aux(_L, A,[A|C1],[A|C1]).
camino_aux(L, A,[Y|C1],C) :-
adyacente(L, X,Y),
not(member(X,[Y|C1])),
camino_aux(L, A,[X,Y|C1],C).