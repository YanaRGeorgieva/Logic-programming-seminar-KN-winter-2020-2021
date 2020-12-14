% АРИТМЕТИКА 
% length, count, n_th_element, ...

% X is Y+Z 

length2([], 0).
length2([_|T], N):-length2(T, M), N is M + 1.

% count(X, L, N).
count(_, [], 0).
count(X, [X|T], N):-count(X, T, M), N is M+1.
count(X, [Y|T], N):-X\=Y, count(X, T, N).


% elementOf(X, N, L). N has a given value for it to work
elementOf(X, 1, [X|_]).
elementOf(X, N, [_|T]):- N > 1, N1 is N - 1, elementOf(X, N1, T).


n_th_element1(X, 1, [X|_]).
n_th_element1(X, N, [_|T]):- n_th_element1(X, M, T), N is M + 1.


% Супер Марио е подложен на поредното предизвикателство. Той се намира на
% надморска височина N единици и пред него е зейнала огромна пропаст, в която
% стърчат метални кулички с различни надморски височини, подредени в редич-
% ка плътно една след друга стигаща до другия край на пропастта. До него има
% ръчка, която при всяко дърпане преподрежда куличките в нова редичка.
% За улеснение нека си представим редицата от куличките като списък, чиито
% елементи са списъци от крайни вложения на празния списък
% ( [[[[[]]]], [[]], [[[[[[]]]]]]] −→ списък с началните разположения на
% кулички с надморски височини с 3, 1 и 5 единици).
% Той може да скача и пропада на височина максимум 2 единици.
% Да се дефинира предикат futureBride(Towers, N, L), който по дадено пред-
% ставяне на редица от кулички Towers и първоначална надморска височина N
% генерира в L чрез преудовлетворяване всички последователности на куличките,
% така че Супер Марио да може да стигне от единия край на пропастта до другия
% и да спаси принцесата от чудовището.

% towersToHights(L, R).
% height(L, N).

height([], 0).
height([L], N):- height(L, M), N is M + 1.

towersToHights([], []).
towersToHights([H|T], [N|R]):- height(H, N), towersToHights(T, R).

condition(L):- not(( append(_, [X, Y|_], L), Z is abs(X - Y), Z > 2 )).

futureBride(Towers, N, L):- towersToHights(Towers, R), permutation(R, L), condition([N|L]).

% ГЕНЕРАТОРИ

% natural, switchSign, integer, odd, between, range, pairs, genKS, genAllFiniteNatNum, fibonacci, split, flatten, generateTree

% ?-p(X), filter(X).

natural(0).
natural(N):- natural(M), N is M + 1.

even(0).
even(N):- even(M), N is M + 2.

even1(N):- natural(M), N is M * 2.

even2(N):- natural(N), 0 =:= N mod 2.

divGen(_, 0).
divGen(N, M):- divGen(N, M1), M is M1 + N.

switchSign(0, 0).
switchSign(X, Y):- X > 0, Y is X.
switchSign(X, Y):- X > 0, Y is -X.

integer1(Z):- natural(N), switchSign(N, Z).

% between(A, B, C). -> A =< B, A, B are integers, C in [A, B] , C is integer
% [A, A+1, A+2, .... B = A + N], N natural.
between1(A, B, A):- A =< B. 
between1(A, B, C):- A < B, A1 is A + 1, between1(A1, B, C).

range(A, B, []):- A > B.
range(A, B, [A|R]):- A =< B, A1 is A + 1, range(A1, B, R).

% N = A + B
pairs(A, B):- natural(N), between1(0, N, A), B is N - A.

genKS(1, S, [S]).
genKS(K, S, [XI|R]):- K > 1, K1 is K - 1, between1(0, S, XI), S1 is S - XI, genKS(K1, S1, R).

% x_1 + x_2 + ... + x_k = S

% x_1 + x_2 = N, N natural
%  X + Y =:= 4*5+1
%   =
%   #=

genAllFiniteNatNum([]).
genAllFiniteNatNum(L):- pairs(K, S), genKS(K, S, L).

% fibonacci
% f(AN-1, AN)
% X, Y
% Y, X + Y= Z
f(0, 1).
f(Y, Z):-f(X, Y), Z is X + Y.
% 0 1
% 1 1
% 1 2
% 2 3
% 3 5

% split, flatten

fibonacci(X):-f(X, _).
% a0=1 a1=2 a3=2
% an+3=an+1 + 10*n -an + 0*an+x_2
% n an an+1 an+2
% an+1 an+2 an+3 = an+1 + 10*n -an + 0*an+x_2
a(0, 1, 2, 2).
a(N, Y, Z, T):-a(M, X, Y, Z), N is M + 1, T is Y + 10*M - X.
a(X):-a(_, X, _, _).

flattenMain(L, R):- is_list(L), flatten(L, R).

flatten(X, [X]):- not(is_list(X)). %[[[[2]]], [4,5], [[[a]]]] -> [2,4,5,a]
flatten([], []).
flatten([H|T], R):- flatten(H, FH), flatten(T, FT), append(FH, FT, R).

% [1,2,3] -> [[1], [2], [3]], 
%             [[1,2], [3]], 
%             [[1], [2, 3]], 
%             [[1,2,3]]

split([], []).
split([H|T], [X|R]):- append(X, L, [H|T]), X\=[], split(L, R).

