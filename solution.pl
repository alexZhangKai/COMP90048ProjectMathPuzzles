%  File    : Proj2.pl
%  Author  : Kai Zhang
%  LMS Id  : kaiz2
%  Purpose : Solve a math puzzle with following constrains.
%  Puzzle Constrains :
%            1. Puzzle elements are between 1 to 9.
%            2. Elements on each row and col is not repeated.
%            3. Diagonal elements have the same value.
%            4. The head of each row/col is either the product or sum of
%               all elements in that row/col.
%  Detail  : The program solves this puzzle by finding and filling a certain
%            row/col with the fewest possibilities. The base case is when all
%            elements are bound and satisfying all constrains.

% load clpfd library
:- ensure_loaded(library(clpfd)).

% The input is a puzzle represented by a list of list.
puzzle_solution(Puzzle) :-
  % check if the puzzle is filled
  append(Puzzle, Elems),
  maplist(ground,Elems),
  % check if the puzzle satisfies all constrains
  Puzzle=[_|Xs],
  maplist(sum_or_product,Xs),
  transpose(Puzzle,Puzzle1),
  Puzzle1=[_|Ys],
  maplist(sum_or_product,Ys).
puzzle_solution(Puzzle) :-
  % check if the matrix is a square
  maplist(same_length(Puzzle), Puzzle),
  % check if the diagonal is the same
  same_diagonal(Puzzle, 1),
  % find a row/col with the least possibility
  next(Puzzle,Next),
  puzzle_solution(Next).

% check if elements on a matrix diagonal are the same
same_diagonal([_], _).
% skip the first row
same_diagonal([_|Xs],1) :-
  same_diagonal(Xs,2).
same_diagonal([X1,X2|Xs], N) :-
  N1 is N+1,
  nth1(N, X1, E),
  nth1(N1, X2, E),
  same_diagonal([X2|Xs], N1).

% check if the first element in a list is the product or
% sum of the rest elements also check domain, equality
sum_or_product([X|Xs]) :-
  maplist(between(1,9),Xs),
  all_distinct(Xs),
  (get_product([X|Xs]);
  get_sum([X|Xs])).

% backtrackable sum
get_sum([A,B,C]) :-
  A #= B+C.
get_sum([A,B,C,D]) :-
  A #= B+C+D.
get_sum([A,B,C,D,E]) :-
  A #= B+C+D+E.

% backtrackable product
get_product([A,B,C]) :-
  A #= B*C.
get_product([A,B,C,D]) :-
  A #= B*C*D.
get_product([A,B,C,D,E]) :-
  A #= B*C*D*E.

% find row with the least possibility
next(MatR,Next) :-
  % get the number of all possibility
  % of every row and col
  MatR=[_|XsR],
  maplist(num_of_comb,XsR,NumsR),
  get_min(MinR,NumsR),
  transpose(MatR,MatC),
  MatC=[_|XsC],
  maplist(num_of_comb,XsC,NumsC),
  get_min(MinC,NumsC),
  % handle row and col separately
  (MinR=<MinC ->
    % find the row with the fewest possibilities
    get_index(Index,NumsR,MinR),
    ActIndex is Index+1,
    nth1(ActIndex,MatR,Target),
    % fill puzzle with this row
    sum_or_product(Target),
    replace(ActIndex,Target,MatR,Next)
  ; % find the col with the fewest possibilities
    get_index(Index,NumsC,MinC),
    ActIndex is Index+1,
    nth1(ActIndex,MatC,Target),
    % fill puzzle with this col
    sum_or_product(Target),
    replace(ActIndex,Target,MatC,NextC),
    transpose(NextC,Next)
  ).

% get the number of possibile ways to fill a row/col
num_of_comb(List,Len) :-
  (maplist(ground,List) ->
    Len = 0
  ;
    bagof(List,sum_or_product(List),R),
    length(R,Len)
  ).

% replace an element in a list
replace(1,Value,[_|Xs],[Value|Xs]).
replace(Index,Value,[X|Xs0],[X|Xs1]) :-
  Index>1,
  Index1 is Index-1,
  replace(Index1,Value,Xs0,Xs1).

% get min in a list but not 0
get_min(Min,List) :-
  delete(List,0,NewList),
  NewList=[X|Xs],
  get_min(Min,X,Xs).
get_min(Min,Min,[]).
get_min(Min,Min0,[X|Xs]) :-
  Min1 is min(Min0,X),
  get_min(Min,Min1,Xs).

% get index of an element without choicepoint
get_index(1,[Target|_],Target) :- !.
get_index(Index,[_|Xs],Target) :-
  get_index(Index1,Xs,Target), !,
  Index is Index1+1.
