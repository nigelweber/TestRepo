

lister([a,b]).


task_letter_value(a,0).
task_letter_value(b,1).
task_letter_value(c,2).
task_letter_value(d,3).
task_letter_value(e,4).
task_letter_value(f,5).
task_letter_value(g,6).
task_letter_value(h,7).
/*
number_letter_rep(0,a).
number_letter_rep(1,b).
number_letter_rep(2,c).
number_letter_rep(3,d).
number_letter_rep(4,e).
number_letter_rep(5,f).
number_letter_rep(6,g).
number_letter_rep(7,h).
*/
adjacent_pair_val(a,b,5).
adjacent_pair_val(c,d,15).


tlist([],[]).
tlist(L,V) :-
	mcalc_near_pen(L,0,Result),
	takelist(L,X),
	list_sum(X,Z),
	V is Z + Result,!.

rlist([],[]).
rlist([H|T],V) :-
	%calc_near_pen([H|T],0,Result),
	takelist([H|T],V),
	%list_sum(X,Z),
	V is Z.
	
list_sum([],0).
list_sum([HR|TR],Sumr) :-
	list_sum(TR,Restt),
	Sumr is HR + Restt.
	
takelist([],[]).

takelist([H|T],[X|Y]) :- 
	task_letter_value(H,X),
	takelist(T,Y).

get_last_element([E],E).
get_last_element([_|T],X) :-
	get_last_element(T,X).

get_head([H|T],N) :-
	N is H.
	
mcalc_near_pen(L,Currentt,R) :-
	calc_near_pen(L,Currentt,Resultt),
	get_last_element(L,Y),
	[Head|_]=L,
	too_near_pen(Y,Head,V),
	R is Resultt + V.

calc_near_pen([],Current,Current).
calc_near_pen([E],Current,Current).	
calc_near_pen([E,E1|Rest],Current,Result) :-
	too_near_pen(E,E1,Val),
	Sum is Current + Val,
	calc_near_pen([E1|Rest],Sum,Result).
	
too_near_pen(X,Y,V) :-
	adjacent_pair_val(X,Y,Z),
	!,
	Z > 0,
	V is Z;
	V is 0.
	
	


gettask_letter_value(X,Z) :-
	task_letter_value(X,V),
	Z is V.	

getlengthlist(L,N) :-
	mylist(L),
	listtask_letter_value(L,N).
	
mylist([a,b,c,d]).
%mylist(L).
	
listtask_letter_value([],0).
listtask_letter_value([H|T],N) :-
	listtask_letter_value(T,N1),
	N is N1 + 1.
	

altlistval(L,N) :- listacc(L,0,N).

listacc([],A,A).
lenacc([H|T],A,N) :-
	A1 is A + 1,
	lenacc(T,A1,N).

	
compared(X,Y,V) :- 
	gettask_letter_value(X,Z),
	gettask_letter_value(Y,W),
	Z = W,
	V is W.
	
compared(X,Y,V) :- 
	gettask_letter_value(X,Z),
	gettask_letter_value(Y,W),
	Z > W,
	V is W.

	
compared(X,Y,V) :-
	gettask_letter_value(X,Z),
	gettask_letter_value(Y,W),
	Z < W,
	V is Z.





	
	


printStuff([H|_]):- write(H).


myprog(X) :- lister(L), member(X, L).





