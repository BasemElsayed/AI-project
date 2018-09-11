test:-
	open('C:/Users/Basem Elsayed/Desktop/20150071&20150080/input.txt', read,S),
	read(S,H1),
    read(S,H2),
	read(S,H3),
	read(S,H4),
	read(S,V1),
	read(S,V2),
	read(S,V3),
	read(S,V4),    	
	close(S),
    path([[[-1,-1,-1,-1,-1,-1,-1,-1],null]],[],[H1,H2,H3,H4,V1,V2,V3,V4]).
	


move([_,_,_,_,_,_,_,_],W,CHILD):-
	permutation(W,CHILD).
printlist([]):-!.
printlist([H|T]):-
write('  '),write(H),write('  '),printlist(T).

goalState([_,R2H1,_,R4H1,_,R6H1,_,R8H1,_], %H1
	 [_,R2H2,_,R4H2,_,R6H2,_,R8H2,_], %H2
	 [_,R2H3,_,R4H3,_,R6H3,_,R8H3,_], %H3
	 [_,R2H4,_,R4H4,_,R6H4,_,R8H4,_], %H4
	 [_,R2H1,_,R2H2,_,R2H3,_,R2H4,_], %V1
	 [_,R4H1,_,R4H2,_,R4H3,_,R4H4,_], %V2
	 [_,R6H1,_,R6H2,_,R6H3,_,R6H4,_], %V3
	 [_,R8H1,_,R8H2,_,R8H3,_,R8H4,_]).%V4
	

path([],_,_):-
	write('Sorry,Solution Not Found.'), open('state.txt',write, Stream), write(Stream,"0"), close(Stream), close(Stream),nl,!.

path([[[H1,H2,H3,H4,V1,V2,V3,V4],_] | _], _, _):-
		atom_chars(H1,WH1),atom_chars(H2,WH2),atom_chars(H3,WH3),atom_chars(H4,WH4),atom_chars(V1,WV1),atom_chars(V2,WV2),atom_chars(V3,WV3),atom_chars(V4,WV4),
		goalState(WH1,WH2,WH3,WH4,WV1,WV2,WV3,WV4),
		write('Solution is : '), nl ,open('output.txt',write, Stream),
		write('          V1  '),write('      V2  '),write('      V3  '), write('      V4  '),nl ,write('          '),nth0(0,WV1,X),write(X),write(Stream,X),write('  '),write('       '),nth0(0,WV2,Y),write(Y), write(Stream, Y),write('  '),write('       '),nth0(0,WV3,Z),write(Z),write(Stream,Z),write('  '),write('       '),nth0(0,WV4,U),write(U),write('  '),write(Stream,U),write(Stream,"$") ,nl,
		write('H1 '),printlist(WH1),nl,write('          '),nth0(2,WV1,A),write(A),write(Stream, A),write('         '),nth0(2,WV2,A1),write(A1),write(Stream,A1),write('         '),nth0(2,WV3,A2),write(A2),write('         '),write(Stream, A2),nth0(2,WV4,A3),write(A3),write(Stream, A3), write(Stream,"$") ,nl,
		write('H2 '),printlist(WH2),nl,
		write('          '),nth0(4,WV1,A4),write(A4),write('         '), write(Stream,A4),nth0(4,WV2,A5),write(A5),write('         '),write(Stream,A5),nth0(4,WV3,A6),write(A6),write('         '),write(Stream,A6) ,nth0(4,WV4,A7),write(A7),write(Stream,A7), write(Stream,"$"),nl,
		write('H3 '),printlist(WH3),nl,
		write('          '),nth0(6,WV1,A8),write(A8),write('         '), write(Stream,A8),nth0(6,WV2,A9),write(A9),write('         '),write(Stream,A9) ,nth0(6,WV3,AX),write(AX),write('         '), write(Stream,AX), nth0(6,WV4,AY),write(AY), write(Stream,AY),nl,
		write('H4 '),printlist(WH4),write(Stream,"$") ,nl,
		write('          '),nth0(8,WV1,AW),write(AW),write(Stream,AW) , write('         '),nth0(8,WV2,AZ),write(AZ),write('         '), write(Stream,AZ),nth0(8,WV3,AU),write(AU),write('         '),write(Stream,AU) ,nth0(8,WV4,AO),write(AO),write(Stream,AO) ,write(Stream,"$"), write(Stream, "#"),write(Stream, WH1),write(Stream, "#"),write(Stream, WH2),write(Stream, "#"),write(Stream, WH3),write(Stream, "#"), write(Stream, WH4), write(Stream, "#") ,close(Stream),nl
		,!.
path(Open, Closed, []):-!,
		removeFromOpen(Open, [State, Parent], RestOfOpen),
		path(RestOfOpen, [[State, Parent] | Closed], []).
		
path(Open, Closed, Words):-
		removeFromOpen(Open, [State, Parent], RestOfOpen),
		getchildren(State, Open, Closed,Words, Children),
		append(Children,RestOfOpen , NewOpen),
		path(NewOpen, [[State, Parent] | Closed], []).


%gets Children of State that arent in Open or Close
getchildren(State, Open ,Closed, Words, Children):-
		bagof(X, moves( State, Open, Closed,Words, X), Children), ! .
getchildren(_,_,_, []).

%adds children to open list (without head child) to form new open list
%here it is like append i.e.Breadth First
addListToOpen(Children, [], Children).
addListToOpen(Children, [H|Open], [H|NewOpen]):-
		addListToOpen(Children, Open, NewOpen).

%gets head of open list to get its children later
removeFromOpen([State|RestOpen], State, RestOpen).

	
%gets next state given the current state
moves( State, Open, Closed,Words,[Next,State]):-
		move(State,Words,Next),
		\+ member([Next,_],Open),
		\+ member([Next,_],Closed).

%prints the path from start state to goal state
printsolution([State, null],_):-
		write(State),nl.
printsolution([State, Parent], Closed):-
		member([Parent, GrandParent], Closed),
		printsolution([Parent, GrandParent], Closed),
		write(State), nl.
