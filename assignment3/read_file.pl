/*program reads in file and outputs it as a list of characters which is stored in the database.
*/

:-dynamic(contents/1, error/1).
:-use_module(parser).
:-use_module(solver).

:- initialization(commandline).
commandline :- argument_value(1, X), argument_value(2, Y), write(X), write(Y), write('\n'), inputoutput(X,Y).
    
/*    X is the file name, Y is the output*/
inputoutput(X,Y):-
  retractall(contents(_)),
  see(X),
  read_file(3,X1),       /*Y1 is the holder of character int code list*/
  seen,
  removeLast(X1, Y1),
  append(Y1,"\n\n\n", Y2),
  asserta(contents(Y2)),
  /*%Y = Y2,
  write_file('writetry.txt',Y).*/
  parse,
  error(Z),
  !,
  parseerrors(Z,Y), %check for errors, include output filename.
  solve,
  error(Z),
  !,
  parseerrors(Z,Y),
  retract(error(_)),
  solutionformat(Out),
  write_file(Y,Out),!.
  

read_file(-1,[]).
read_file(_,Y):-
    get0(Z),
    read_file(Z,W),
    Y = [Z|W].
    
parseerrors(nil, _).
parseerrors(invalidPartialAssignment, X):-
  write_file(X,"partial assignment error"),
  fail.
parseerrors(invalidMachineTask, X):-
  write_file(X,"invalid machine/task"),
  fail.
parseerrors(invalidMachinePenalty, X):-
  write_file(X,"machine penalty error"),
  fail.
parseerrors(invalidTask, X):-
  write_file(X,"invalid task"),
  fail.
parseerrors(invalidPenalty, X):-
  write_file(X,"invalid penalty"),
  fail.
parseerrors(parseErr, X):-
  write_file(X,"Error while parsing input file"),
  fail.
parseerrors(noValidSolution, X):-
  write_file(X,"No valid solution possible!"),
  fail.
  
solve:-
  asserta(error(nil)),
  asserta(solver_solution([a,b,d,c,f,g,e,h,99])).
  
solutionformat(FinalOutput):-
  Output1 = "Solution ",
  solver_solution(X),
  getelement(1,X,Y1),
  atom_codes(Y1,Z1),
  append(Output1,Z1,Output2),
  getelement(2,X,Y2),
  atom_codes(Y2,Z2),
  append(Output2,Z2,Output3),
  getelement(3,X,Y3),
  atom_codes(Y3,Z3),
  append(Output3,Z3,Output4),
  getelement(4,X,Y4),
  atom_codes(Y4,Z4),
  append(Output4,Z4,Output5),
  getelement(5,X,Y5),
  atom_codes(Y5,Z5),
  append(Output5,Z5,Output6),
  getelement(6,X,Y6),
  atom_codes(Y6,Z6),
  append(Output6,Z6,Output7),
  getelement(7,X,Y7),
  atom_codes(Y7,Z7),
  append(Output7,Z7,Output8),
  getelement(8,X,Y8),
  atom_codes(Y8,Z8),
  append(Output8,Z8,Output9),
  append(Output9,"; Quality: ",Output10),
  getelement(9,X,Y9),
  number_codes(Y9,Z9),
  append(Output10,Z9,FinalOutput), !.

getelement(1,[H|_],H).
getelement(X,[_|T],Z):-
  Y is X - 1,
  getelement(Y,T,Z).
  
  
/*knowledge base contains a predicate of output
 *for now: X = file name, Y = string to write*/
write_file(X,Y):-
  tell(X),
  outputstuff(Y),
  told,!.
/*  
outputstuff([]).
outputstuff([Head|Tail]):-
  atom_codes(X,[Head]),
  write(X),
  outputstuff(Tail).*/

outputstuff(Y):-
  atom_codes(X,Y),  
  write(X).