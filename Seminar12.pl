/* 
Да се дефинира на пролог предикат p(X, Y ),
който по даден списък X от списъци от числа намира та-
къв елемент Y на X, че Y не съдържа по-голям елемент от
най-големите елементи на елементите на X, и никой еле-
мент на X, притежаващ същото свойство, не е с повече
елементи от Y .


Да се дефинира на пролог предикат p(L), който
по даден списък от различни списъци L проверява дали
в L съществуват два различни елемента, които имат общ
елемент, който не принадлежи на никой друг елемент на L.


Дърво се нарича краен неориентиран свързан и ацикличен граф. За един спи-
сък от списъци [V, E] ще казваме, че представя неориентирания граф G, ако
V е списък от всички върхове на G и {v, w} е ребро в G тогава и само тогава,
когато [v, w] или [w, v] e eлeмeнт на E.
Да се дефинира на пролог предикат art_tree(V, E)/arc_tree(V, E), който по
дадено представяне [V, E] на краен неориентиран граф разпознава дали има
такава двойка върхове v и w, че [V, E + [v, w]]/[V, E − [v, w]] да е представяне
на дърво, където E + [v, w]/E − [v, w] е списъкът, получен от E с премахването
на всички срещания на елемента [v, w]/добавянето на нов елемент [v, w].


Kaзваме, че списъкът X e екстерзала/екстерзана за списъка от списъци Y ,
ако X има поне един общ елемент с всички елементи на Y и поне два общи
елемента с нечетен/четен брой елементи на Y Да се дефинира на пролог дву-
местен предикат екстерзала (X, Y ), който по даден списък от списъци Y при
презадоволяване генерира всички екстерзали/екстервали X за Y с възможно
най-малка дължина и спира.


Представяне на кореново дърво T ще наричаме всяка двойка от (V, E), където
V е списък от върховете на T , а E е списък от точно онези двойки (u, v), за
които u е баща на v в T .
За кореново дърво T = (V, E) ще казваме, че балансирано/дълбоко, ако ви-
сочината му не надвишава 2 log2 |V |/ log 2 2|V | .
Да се дефинира на пролог предикат balanced(T )/deep(T ), който по предста-
вяне на кореново дърво проверява дали то е балансирано/дълбоко.


Казваме, че списъкът X от естествени числа e:
1. вариант 1: контракуляр за списъка от списъци от естествени числа Y ,
ако всеки елемент на Y съдържа елемент, който се дели без остатък от
всички елементи на X, а всеки елемент на X се дели без остатък от някой
елемент на елемент на Y .
2. вариант 2: кентракуляр за списъка от списъци от естествени числа Y ,
ако всеки елемент на Y съдържа елемент, който дели без остатък някой
елемент на X, а всеки елемент на X дели без остатък някой елемент на
елемент на Y .
Да се дефинира предикат p(X, Y ), който по даден списък от списъци от
естествени числа Y намира контракуляр X с възможно най-много различни
елементи.


За естествено число n с представяне в 8-ична бройна система n = P
i=0 ∞
 ai 8i
 , където ai ∈
{0, 1, 2, 3, 4, 5, 6, 7}, ще казваме, че е байт-дървовидно тогава и само тогава, когато ако:
• Вариант 1: v = bu/8c и au 6≡ 0 (mod 7), то au ≡ av + 1 (mod 6).
• Вариант 2: v = bu/8c и au 6≡ 0 (mod 6), то av ≡ au + 1 (mod 7).
Да се дефинира на предикат на пролог byteT reeN um(N ), който по дадено естествено число
N проверява дали то е байт-дървовидно.


Ще казваме, че един списък е анаграма на друг, ако е съставен от същите елементи, но
в евентуално различен ред.
Да се дефинира предикат на пролог maxAnagrams(L, M ) на пролог, който по даден
списък от списъци L, генерира в M най-голямото число, за което има поне M на брой:
• M -елементни списъка от L, които са анаграми един на друг.
• (M + 2)-елементни списъка от L, които са анаграми един на друг.


I.1 Редиците на Фарей, Fn , са редици от двойки естествени числа, които
се дефинират рекурсивно за n ≥ 1 по следния начин:
• F1 = [[0, 1], [1, 1]];
• Fn+1 се получава от Fn , като между всеки два последователни члена
[a, b] и [c, d] на Fn, за които b+d = n+1, се добавя двойката [a+c, n+1].
Да се дефинира на пролог едноместен предикат farey(F), който при пре-
удовлетворяване генерира в F всички редици на Фарей.


Да се дефинира на Пролог двуместен предикат,
който по дадени две цели числа A, B in Z разпознава дали те имат
едни и същи прости делители.


Казваме, че списък X мажорира списък Y , ако
всички елементи на X са елементи на Y . Да се дефинира
на пролог предикат p(L, M ), който по даден списък от спи-
съци L намира списък M , който съдържа всички елементи
на L и в който никой елемент не се мажорира от елемент,
намиращ се след него в списъка.


Ще казваме, че един списък е анаграма на друг, ако е съставен от същите елементи, но
в евентуално различен ред.
Да се дефинира предикат на пролог maxAnagrams(L, M ) на пролог, който по даден
списък от списъци L, генерира в M най-голямото число, за което същ K >= M на брой
M-елементни списъка от L, които са анаграми един на друг.
*/

% ГРАФИ
/*
    - Представяния
    - simplePath
    - hasCycle, isClique, isConnected, hasIsolated, isTransitive, isMultiGraph, isHamiltonian, maxClique, dfs, bfs, spanningTree, generateCriticalVerteces 
*/

% V = [a,b,s,k,s,g, ...]
% E = [[a,b], [d,s], ...]

edge(U, W, E):-  member([U, W], E); member([W, U], E).

% (forall W)(forall U) [path(W, U)] |=| ~(exists W)(exists U) [~path(W, U)]
isConnected([V, E]):- not(( member(W, V), member(U, V), W \= U, not(simplePath([V, E], W, U, _)))).

simplePath(G, W, U, Path):- simplePath1(G, W, [U], Path).

simplePath1([_, _], U, [U|Path], [U|Path]).
simplePath1([V, E], W, [U|Rest], Path):- edge(T, U, E), not(member(T, [U|Rest])), simplePath1([V, E], W, [T, U|Rest], Path).

simplePath2([V, E], U, W, Path):- simplePath3([V, E], [U], W, Path1), reverse(Path1, Path).
simplePath3([_, _], [W|Path], W, [W|Path]).
simplePath3([V, E], [U|Rest], W, Path):- edge(U, T, E), not(member(T, [U|Rest])), simplePath3([V, E], [T, U|Rest], W, Path).


% dfs(X):-success(X).
% dfs(X):-edge(X, Y), dfs(Y).

graph([[1, 2, 3, 4, 5], [[1, 2], [2, 3], [3, 1], [3, 4]]]).

:- table isThereAPath(+,+,+).
isThereAPath(_, W, W).
isThereAPath([V, E], U, W):- edge(U, T, E), isThereAPath([V, E], T, W).

hasCycle([V, E], Path1):- member(U, V), simplePath([V, E], U, W, Path), 
                            edge(U, W, E), length(Path, N), N > 2, append(Path, [U], Path1). 

:- use_module(library(clpfd)). % -> q / r
% constraint logic programming over finite domains
/* https://www.swi-prolog.org/man/clpfd.html
http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html
https://www.metalevel.at/prolog/clpz
https://github.com/triska/clpz
https://www.swi-prolog.org/pldoc/man?section=summary-lib-clpfd */

% #/\/2	P and Q hold.
% #</2	The arithmetic expression X is less than Y.
% #<==/2	Q implies P.
% #<==>/2	P and Q are equivalent.
% #=/2	The arithmetic expression X equals Y.
% #=</2	The arithmetic expression X is less than or equal to Y.
% #==>/2	P implies Q.
% #>/2	Same as Y #< X.
% #>=/2	Same as Y #=< X.
% #\/1	Q does _not_ hold.
% #\/2	Either P holds or Q holds, but not both.
% #\//2	P or Q holds.
% #\=/2	The arithmetic expressions X and Y evaluate to distinct integers.
% all_different/1	Like all_distinct/1, but with weaker propagation.
% all_distinct/1	True iff Vars are pairwise distinct.
% automaton/3	Describes a list of finite domain variables with a finite automaton.
% automaton/8	Describes a list of finite domain variables with a finite automaton.
% chain/2	Zs form a chain with respect to Relation.
% circuit/1	True iff the list Vs of finite domain variables induces a Hamiltonian circuit.
% cumulative/1	Equivalent to cumulative(Tasks, [limit(1)]).
% cumulative/2	Schedule with a limited resource.
% disjoint2/1	True iff Rectangles are not overlapping.
% element/3	The N-th element of the list of finite domain variables Vs is V.
% fd_dom/2	Dom is the current domain (see in/2) of Var.
% fd_inf/2	Inf is the infimum of the current domain of Var.
% fd_size/2	Reflect the current size of a domain.
% fd_sup/2	Sup is the supremum of the current domain of Var.
% fd_var/1	True iff Var is a CLP(FD) variable.
% global_cardinality/2	Global Cardinality constraint.
% global_cardinality/3	Global Cardinality constraint.
% in/2	Var is an element of Domain.
% indomain/1	Bind Var to all feasible values of its domain on backtracking.
% ins/2	The variables in the list Vars are elements of Domain.
% label/1	Equivalent to labeling([], Vars).
% labeling/2	Assign a value to each variable in Vars.
% lex_chain/1	Lists are lexicographically non-decreasing.
% scalar_product/4	True iff the scalar product of Cs and Vs is in relation Rel to Expr.
% serialized/2	Describes a set of non-overlapping tasks.
% sum/3	The sum of elements of the list Vars is in relation Rel to Expr.
% tuples_in/2	True iff all Tuples are elements of Relation.
% zcompare/3	Analogous to compare/3, with finite domain variables A and B.

length2([], N):- N #= 0.
length2([_|T], N):- M #>= 0, N #= M+1, length2(T, M).

/* 
X in A..B -> singleton=5 / 3..7/ inf/number..sup/number / Dom1 \/ Dom2 -> 2..3 \/ 14..90
label([X]).
[H|T] ins A..B същото е като първото, но за списък [H|T]
all_distinct(L)
 */

between1(A, B, C):- C in A..B, label([C]).

between2(A, B, C):- A #=< B, C #= A.
between2(A, B, C):- A #< B, between2(A+1, B, C).

nthElement([H|_], N, H):- N #= 0.
nthElement([_|T], N, R):- N #> 0, nthElement(T, N-1, R).

range(A, B, R):-length2(R, B-A+1), R ins A..B, all_distinct(R), label(R). 

perm(L, P):-length2(L, N), N1 #= N-1, range(0, N1, R), maplist(nthElement(L), R, P).


naty(0).
naty(N):- naty(M), N is M + 1.
% [], [A, B] A, B are trees
% ?-tree(X).
tree([], N):-N #= 0.
tree([A, B], N):-N #> 0, N #=M+K+1, M#>=0, K#>=0, tree(A, M), tree(B, K).

tree(T):-naty(N), tree(T, N).


% multiSubSet(L, M).
% WRONG 
/* multiSubset(_, []).
multiSubset(L, [H|T]):- member(H, L), multiSubset(L, T). */


multiSubset(_, [], N):- N #=0.
multiSubset(L, [H|T], N):-N #> 0, member(H, L), multiSubset(L, T, N-1).

multiSubset(L, R):- naty(N), multiSubset(L, R, N).

multiSubset1(_, []).
multiSubset1(L, [H|T]):- multiSubset1(L, T), member(H, L).