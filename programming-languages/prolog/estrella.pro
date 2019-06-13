estrella([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):-
	esTreintayCuatro(B,C,D,E,[]),
	esTreintayCuatro(L,M,N,O,[B,C,D,E]),
	esTreintayCuatro(B,F,J,L,[C,D,E,M,N,O]),
	esTreintayCuatro(E,G,K,O,[B,C,D,L,M,N,F,J]),
	esTreintayCuatro(A,C,F,H,[B,D,E,L,M,N,O,J,G,K]),
	esTreintayCuatro(A,D,G,I,[B,C,E,F,H,J,K,L,M,N,O]),
	esTreintayCuatro(H,J,M,P,[A,B,C,D,E,F,G,I,K,L,N,O]),
	esTreintayCuatro(P,N,K,I,[]).

esTreintayCuatro(A,B,C,D,L2):- 
	L = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],

	member(A,L),
	member(B,L),
	member(C,L),	  
	member(D,L),
		
	A\=B,
	B\=C,
	C\=D,
	
	\+ member(A,L2),
	\+ member(B,L2),
	\+ member(C,L2),	  
	\+ member(D,L2),
	
	A+B+C+D=:=34.