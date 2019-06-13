punto(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lectura

leer(L,M,N):-
	write('Por favor, introduzca el nombre del archivo'), nl,
	read(Filename),
	open(Filename,read,Stream),
	read_line_to_codes(Stream,Lm),
	number_codes(M,Lm),
	leerInter(Stream,N,P),
	extraerPuntos(0,P,L),
	close(Stream).
	
leerInter(Stream,0,[]) :-
    at_end_of_stream(Stream),!.

leerInter(Stream,N,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,X),
    leerInter(Stream,N1,L),
	N is N1+1.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% Procesamiento de entrada en Puntos
	
extraerPuntos(_,[],[]):-!.

extraerPuntos(M,[[L|L1]|Ls],P):-
	extraerFila(M,0,[L|L1],P1),
	M1 is M+1,
	extraerPuntos(M1,Ls,P2),
	append(P1,P2,P).
	
extraerFila(_,_,[],[]):-!.

extraerFila(M,N,[L|Ls],[punto(N,M)|Ps]):-
	N1 is N+1,
	L == 32,
	extraerFila(M,N1,Ls,Ps),!.

extraerFila(M,N,[L|Ls],Ps):-
	N1 is N+1,
	\+ L == 32,
	extraerFila(M,N1,Ls,Ps).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Resuelve el laberinto
	
resolver(L,M,N,F):-
	resolverInter(L,0,0,M,[],F).
	
resolverInter(L,X,Y,M,Acc,R):-
	member(punto(X,Y),L),
	X =:= M-1 ,
	reverse([punto(X,Y)|Acc],R),!.

resolverInter(L,X,Y,M,Acc,R):-
	member(punto(X,Y),L),
	\+ member(punto(X,Y),Acc),
	Xn is X +1,
	Yn = Y,
	resolverInter(L,Xn,Yn,M,[punto(X,Y)|Acc],R),!.

resolverInter(L,X,Y,M,Acc,R):-
	member(punto(X,Y),L),
	\+ member(punto(X,Y),Acc),
	Yn is Y +1,
	Xn = X, 
	resolverInter(L,Xn,Yn,M,[punto(X,Y)|Acc],R),!.
	
resolverInter(L,X,Y,M,Acc,R):-
	member(punto(X,Y),L),
	\+ member(punto(X,Y),Acc),
	Xn is X -1,
	Yn = Y,
	resolverInter(L,Xn,Yn,M,[punto(X,Y)|Acc],R),!.

resolverInter(L,X,Y,M,Acc,R):-
	member(punto(X,Y),L),
	\+ member(punto(X,Y),Acc),
	Yn is Y -1,
	X = Xn,
	resolverInter(L,Xn,Yn,M,[punto(X,Y)|Acc],R),!.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Escribe el laberinto y la solucion en pantalla.
	
escribir(L,C,M,N):-
	escribirInter(L,C,0,0,M,N).
		
escribirInter(_,_,_,M,M,_):-!.

escribirInter(L,C,N,Y,M,N):-
	Yn is Y+1,
	nl,
	escribirInter(L,C,0,Yn,M,N), !.

escribirInter(L,C,X,Y,M,N):-
	(member(punto(X,Y),L)
	-> (member(punto(X,Y),C)
		  -> write('.')
		  ; write(' '))
	; write('#')),
	Xn is X+1,
	escribirInter(L,C,Xn,Y,M,N), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Prueba todo

probar(R,M,N):-
	leer(L,M,N),
	resolver(L,M,N,R),
	escribir(L,R,M,N).
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	