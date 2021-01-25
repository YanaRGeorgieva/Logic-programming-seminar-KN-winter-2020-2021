condition(X, Y):- condition1(X, Y), condition2(X, Y).

condition1(X, Y):- 
        not((member(A, Y), 
            not((member(B, X), member(B, A))))).

condition2(X, Y):- countCond(X, Y, N), N mod 2 =:= 1.

condition3(X, A):- 
    append(_, [B|L], X), member(C, L), 
        member(B, A), member(C, A).

countCond(_, [], 0).
countCond(X, [A|T], N1):- 
    condition3(X, A), countCond(X, T, N), N1 is N + 1.
countCond(X, [A|T], N):- 
    not(condition3(X, A)), countCond(X, T, N).

exterzala(Y, X):- 
    generateCandidateExterzala(Y, X), condition(X, Y), length(X, LX), 
        not(( generateCandidateExterzala(Y, Z), condition(Z, Y),
                length(Z, LZ), LZ > LX )).

appendAll([], []).
appendAll([H|T], R):- appendAll(T, R1), append(R1, H, R).

subsequence([], []).
subsequence([H|T], [H|R]):- subsequence(T, R).
subsequence([_|T], R):- subsequence(T, R).

generateCandidateExterzala(Y, X):- 
    appendAll(Y, UnionY), subsequence(UnionY, X).

/* Писмен изпит 9 сеп 2015 зад2 - за кодичните списъци, сега ще се пробвам да сложа условието
Ще казваме, че списък е кодичен, ако първият му еле- мент е 0, последният — 1 и всички останали елементи са кодични списъци. Ще казваме, че списък от нули и единици е кодиране- то на даден кодичен списък, ако може да се получи като изтри- ем квадратните скоби в записа на кодичния списък. Например списъкът [0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1] е кодирането на кодичния списък [0, [0, [0, 1], [0, [0, 1], 1], 1], [0, 1], 1]. Да се дефинира преди- кат q(X, Y ), който по дадено кодиране X намира
кодичния спи- сък Y , чието кодиране е X */