carril(brussel,charleroi).
carril(brussel,haacht).
carril(haacht,mechelen).
carril(mechelen,berchem).
carril(berchem,antwerpen).
carril(brussel,boom).
carril(boom,antwerpen).
carril(antwerpen,turnhout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado que define una Ciudad Grande

ciudad_grande(C):-
	listaAdyacencia(C,L),
	length(L,N),
	N > 2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado que defina una lista de ciudades adyacentes a una dada

listaAdyacencia(C,L):-
	bagof(X,listaAdyacenciaInterna(C,X),L).	
		
listaAdyacenciaInterna(C,X):-
	carril(C,X).

listaAdyacenciaInterna(C,X):-
	carril(X,C).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% Predicado que define una Ciudad Pequenia

ciudad_pequenia(C):-
	listaAdyacencia(C,L),
	length(L,N),
	N < 3.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado que define una Buena Ciudad

ciudad_buena(C):-
	setof(X,ciudad_buenaMid(X),L),
	member(C,L).

ciudad_buenaMid(C):-
	setof([X,N],ciudad_buenaInter(X,N),L),
	member(Y, L),
	member(C,Y),
	\+ integer(C).

ciudad_buenaInter(C,N):-
	ciudad_pequenia(C),
	ciudad_grande(X1),
	ciudad_grande(Y1),
	X1 \= Y1,
	listaAdyacencia(C,L),
	member(X2, L),
	member(Y2, L),
	X2 \= Y2,
	dist(C,X2,X1,N),
	dist(C,Y2,Y1,N).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Indica el tamaño de la lista mas corta en una lista de listas

minPath([L|Ls],N):-
	length(L,N2),
	M is N2,
	minPathInter(Ls,M,N).

minPathInter([],M,N):-
	N is M.
	
minPathInter([L|Ls],M,N):-
	length(L,N2),
	N2 < M,
	minPathInter(Ls,N2,N), !.

minPathInter([_|Ls],M,N):-
	minPathInter(Ls,M,N), !.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Indica la distancia minima entre 2 ciudades.

dist(_,A,A,0):- !.

dist(C,A,B,N):-
	bagof(L2,distInter(A,B,[A,C],L2),L),
	minPath(L,N),!.

distInter(A,B,Acc,[B]):-
	listaAdyacencia(A,L),
	\+ member(B,Acc),
	memberchk(B,L).
	
distInter(A,B,Acc,[X|Rest]):-
	listaAdyacencia(A,L),
	member(X,L),
	\+ member(X,Acc),
	distInter(X,B,[X|Acc],Rest).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado que define la Mejor Ciudad

ciudad_mejor(C):-
	setof([X,N],ciudad_buenaInter(X,N),L),
	minDist(L,[C,_]).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado que indica la tupla [ciudad,distancia] con la distancia mas corta.

minDist([[C,D]|Ls],N):-
	minDistInter(Ls,[C,D],N).

minDistInter([],M,M).
	
minDistInter([[C,D]|Ls],[_,D2],N):-
	D < D2,
	minDistInter(Ls,[C,D],N), !.

minDistInter([_|Ls],M,N):-
	minDistInter(Ls,M,N), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado que indicala me mejor estacion.

estacion(C):-
	ciudad_mejor(C).







	