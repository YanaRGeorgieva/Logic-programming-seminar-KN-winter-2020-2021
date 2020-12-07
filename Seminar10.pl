% HW reverse linear complexity, sort, palindrome, isSorted, lessEq(A, B) <, =<, >=, >, =:=, not - A E

% [H|L] S -> L [H|S]
% 1, 2, 3 -> [1] -> [2, 1] -> [3, 2, 1]

reverse2(L, R) :- reverse1(L, [], R).

reverse1([], Stack, Stack).
reverse1([H|T], Stack, Result):- reverse1(T, [H|Stack], Result).

% X, Y - lists, p(, )
% - съществува елемент на X, който е в елемент на Y
% - съществува елемент на X, който е във всеки елемент на Y
% - всеки елемент на X е в елемент на Y
% - всеки елемент на X e във всеки елемент на Y

% EAEB f(A, B)
p1(X, Y):-member(A, X), member(B, Y), member(A, B).
% EA AB f(A, B)
% EA !EB! f(A, B)
p2(X, Y):-member(A, X), not((member(B, Y), not(member(A, B)))).
% AXEYf(A, B)
% !EX!EYf(A, B)
p3(X, Y):-not((member(A, X), not((member(B, Y), member(A, B))))).
% AAAB f(A, B)
% !EA!!EB!f(A, B)
% !EAEB! f(A, B)
p4(X, Y):-not((member(A, X), member(B, Y), not(member(A, B)))).

% AXEY(p(X)&p(Y))
% EYAX(p(X)&p(Y))

pp2(X, Y):-member(A, X), inAll(A, Y).

inAll(_, []).
inAll(A, [H|T]):-member(A, H), inAll(A, T).


lessEq(A, B):-A =< B.

isSorted([]).
isSorted([_]).
isSorted([H1, H2|T]):-lessEq(H1, H2), isSorted([H2|T]).

isSorted2(L):-not((append(_, [A, B|_], L), not(lessEq(A, B)))).

sort1(L,S):-permutation(L,S), isSorted(S).

% - A--< X =< -- B --
% split(X, L, A, B)

split(_, [], [], []).
split(X, [H|T], [H|A], B):- lessEq(H, X), split(X, T, A, B).
split(X, [H|T], A, [H|B]):- not(lessEq(H, X)), split(X, T, A, B).

qsort([], []).
qsort([X|L], S):- split(X, L, A, B), qsort(A, SA), qsort(B, SB), append(SA, [X|SB], S).

% empty
% tree(Root, Left, Right)

% tsort(L, S).
% makeTree(L, T)
% add(X, T, NewT).
% traverse(T, L).

add(X, empty, tree(X, empty, empty)).
add(X, tree(Root, Left, Right), tree(Root, NewLeftTree, Right)):- 
    lessEq(X, Root), add(X, Left, NewLeftTree).
add(X, tree(Root, Left, Right), tree(Root, Left, NewRightTree)):- 
    not(lessEq(X, Root)), add(X, Right, NewRightTree).

makeTree2([], empty).
makeTree2([Root|T], NewTree):- makeTree2(T, TreeTail), add(Root, TreeTail, NewTree).

makeTree([], empty).
makeTree([Root|T], tree(Root, Left, Right)):- length(T, N), N1 is N div 2, append(TL, TR, T), length(TL, N1), 
                                                makeTree(TL, Left), makeTree(TR, Right).

traverse(empty, []).
traverse(tree(Root, Left, Right), Result):- traverse(Left, LeftList), traverse(Right, RightList),
        append(LeftList, [Root|RightList], Result).

tsort(L, S):- makeTree2(L, T), traverse(T, S).
% HW use add, makeTree and traverse to write tsort1(L, S):- 

minElement([X], X).
minElement([H|T], H):- minElement(T, Y), lessEq(H, Y).
minElement([H|T], Y):- minElement(T, Y), not(lessEq(H, Y)).

minEl(X, Y, X):- lessEq(X, Y).
minEl(X, Y, Y):- not(lessEq(X, Y)).

minElement1([X], X).
minElement1([H|T], R):- minElement1(T, Y), minEl(H, Y, R).

minElement2(L, Min):- member(Min, L), not((member(X, L), not(lessEq(Min, X)))). % X < Min
