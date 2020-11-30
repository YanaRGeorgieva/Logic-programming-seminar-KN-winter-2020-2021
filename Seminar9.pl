% append(A, B, AB) [1, 2] [3, 5, 8] [1, 2, 3, 5, 8]
append([], B, B).
append([X|A], B, [X|C]):-append(A, B, C).

% [1, 2, 3, 5, 8]

% ------- X -------

member2(X, L):-append(_, [X|_], L).

% first(F, L)
first(F, [F|_]).
first2(F, L):-append([F], _, L).
first3(X, [H|_]):- X = H.

% last(X, L).
last(X, [X]).
last(X, [_|T]):- last(X, T).

last2(X, L):- append(_, [X], L).

% insert(X, L, L1). 
% insert(1, [a,b], L) -> L=[1,a,b]; L=[a,1,b]; L=[a,b,1]

insert(X, L, L1):- append(A, B, L), append(A, [X|B], L1).

insert1(X, L, [X|L]).
insert1(X, [H|L], [H|M]):-insert1(X, L, M).

remove(X, M, L):-insert1(X, L, M).

member4(X, L):-remove(X, L, _).
% [1,2,3,4,5,1,2,3,4]

% A P
% ---- A ---- = B

permute([], []).
permute(B, [A|P]):- remove(A, B, X), permute(X, P).

permute1([], []).
permute1([H|T], R):- permute1(T, Q), insert(H, Q, R).

prefix(P, L):- append(P, _, L).
suffix(S, L):- append(_, S, L).
infix1([], _).
infix1(I, L):- suffix(S, L), prefix(I, S), I \= [].
infix2(I, L):- prefix(P, L), suffix(I, P).

% ! NB infix3(I, L):-append(_, I, C), append(C, _, L).

% 1,2,3,4,5 -> 3,4?
% 1,2,3,4,5 -> 1,3,4
subsequence([], []).
subsequence([_|T], R):- subsequence(T, R).
subsequence([H|T], [H|R]):- subsequence(T, R).

subsequence2(_, []).
subsequence2(L, [X|S]):-suffix([X|B], L), subsequence2(B, S).
 
isSubset([], _).
isSubset([X|A], B):-member(X, B), isSubset(A, B).

toSet([], []).
toSet([X|T], S):-member(X, T), toSet(T, S).
toSet([X|T], [X|S]):-not(member(X, T)), toSet(T, S).

toSet1([], []).
toSet1([X|T], R):- toSet1(T, R), member(X, R).
toSet1([X|T], [X|R]):- toSet1(T, R), not(member(X, R)).

% a,b,c -> c,b,a
reverse1([], []).
reverse1([H|T], R):- reverse1(T, R1), append(R1, [H], R).

% HW reverse linear complexity, sort, palindrome, isSorted, less(A, B) <, =<, >=, >, =:=


