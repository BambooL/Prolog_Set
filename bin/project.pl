%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sets in Prolog
% Author Hsyao Liu
% Pennsylvania State University
% Set is a sorted List
% Implement the following set operations in Prolog:

 % Union (20 points)
 % union(A,B,R) where R is the union of sets A and B. That is, R contains
 % elements that are in either A or B.

 % Intersection (20 points)
 % intersection(A,B,R) where R is the intersection of sets A and B. That
 % is, R contains elements that are both in A and B.  

 % Difference (20 points)
 % difference(A,B,R) where R is A-B (subtraction or difference). That is,
 % R contains all elements of A that are not in B.

 % Subset (20 points)
 % subset(A,B) succeeds when A is a subset of B.

 % selection (extra credit: 20 points):
 % selection(A,Cond,R), where A is the original set, Cond is the criteria
 % for selecting an element, and R is the resulted set containing all
 % selected elements from A.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % Initialization

set([]).
set([_|_]).
   
lt(X,Y):-var(X);var(Y).
lt(X,Y):-nonvar(X),nonvar(Y),X<Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
  % Membership (20 points)
  % in(X,A) succeeds when an element X is in set A.

in([],_) :-
	fail.
in(X,[]) :-
	fail.
in(X,[X|T]).
in(X,[H|T]):-
	in(X,T).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Intersection (20 points)
  % intersection(A,B,R) where R is the intersection of sets A and B. That
  % is, R contains elements that are both in A and B.  

% empty set exist
intersection([], _, []).
intersection(_, [], []).

% H1 in L2
intersection([H1|T1], L2, [H1|T3]) :-
    in(H1, L2),
    intersection(T1, L2, T3).

% H1 not in L2
intersection([H1|T1],[H2|T2],T3):-
   lt(H1,H2),
   intersection(T1,[H2|T2],T3).

intersection([H1|T1],[H2|T2],T3):-
   lt(H2,H1),
   intersection([H1|T1],T2,T3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Union (20 points)
  % union(A,B,R) where R is the union of sets A and B. That is, R contains
  % elements that are in either A or B.

% empty set
union([],L,L).
union(L,[],L):-
    L\=[].
% H1=H2
union([H1|T1],[H1|T2],[H1|T3]):-
   union(T1,T2,T3).

% H1\=H2
union([H1|T1],[H2|T2],[H1|T3]):-
   lt(H1,H2),
   union(T1,[H2|T2],T3).

union([H1|T1],[H2|T2],[H2|T3]):-
   lt(H2,H1),
   union([H1|T1],T2,T3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Difference (20 points)
  % difference(A,B,R) where R is A-B (subtraction or difference). That is,
  % R contains all elements of A that are not in B.

% empty set
difference([],L,[]).
difference(L,[],L):-
    L\=[].

% H1=H2
difference([H1|T1],[H1|T2],T3):-
   difference(T1,T2,T3).

% H1\=H2
difference([H1|T1],[H2|T2],[H1|T3]):-
   lt(H1,H2),
   difference(T1,[H1|T2],T3).
difference([H1|T1],[H2|T2],T3):-
   lt(H2,H1),
   difference([H1|T1],T2,T3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Subset (20 points)
  % subset(A,B) succeeds when A is a subset of B.

% empty set
subset([],L).

% H1=H2
subset([H1|T1],[H1|T2]):-
    subset(T1,T2).

% H1\=H2
subset([H1|T1],[H2|T2]):- 
    lt(H2,H1),subset([H1|T1],T2).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % selection (extra credit: 20 points):
  % selection(A,Cond,R), where A is the original set, Cond is the criteria
  % for selecting an element, and R is the resulted set containing all
  % selected elements from A.

% empty set
select([],X,lt,[]).

% set is not empty
select([H|T],X,lt,[H|R]):-
    Cond =.. [lt,H,X],
    Cond,
    select(T,X,lt,R).

select([H|T],X,lt,R):-
    Cond =.. [lt,H,X],
    \+Cond,
    select(T,X,lt,R).

% empty set
select([],X,lt,[]).

% set is not empty
select([H|T],X,gt,[H|R]):-
    Cond =.. [gt,H,X],
    Cond,
    select(T,X,gt,R).

select([H|T],X,gt,R):-
    Cond =.. [gt,H,X],
    \+Cond,
    select(T,X,gt,R).


%select([H|T],Cond,[R]):-
%    \+call(Cond),
%    select(T,Cond,[R]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


