/*
Some links:
    https://www.swi-prolog.org/
    http://www.learnprolognow.org/lpnpage.php?pageid=top
    https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
    http://cs.union.edu/~striegnk/courses/esslli04prolog/index.php
    http://kti.ms.mff.cuni.cz/~bartak/prolog.old/learning.html
*/
/* Променливи: Maria, X, Y, This_lalalalalallala, _ 
Константи: атоми и числа:
атом: 'нещо такова', this_atom, a, +-*>; спец: [], {}, ;, !
числа: integer, floating point
Съставни термове: foo(h(0),baz(0)), foo(a)
Списъци (специални термове): [], (X-element, Y-list -> [X|Y])
Съждителни връзки: (, -> LogicAnd); (; -> LogicOr); (not(unary) -> LogicNot).
Предикати: започват с малки букви p(x, y), lessEq(X, Y).
*Всички клаузи завършват на точка!!!!!!!
S |= f

3 вида клаузи(хорнови): 
- факти: p(x, y).; p(X,Y).
- правила: p(   ):-p1(), p2(), ..., pn().
- цели: ?-p1(), p2(), ...,pn(). */

parent(maria, ivan).
parent(ivan, hristo).
parent(tania, hristo).
parent(tania, petar).

/*
maria
  |
ivan   tania
  |   /   \
hristo    petar
*/

grandParent(X, Y):- parent(X, Z), parent(Z, Y).

sibling(X, Y):-parent(Z, X), parent(Z, Y).

ancestor(X, Y):- parent(X, Y).
ancestor(X, Y):- parent(Z, Y), ancestor(X, Z).

ancestor2(X, Y):- parent(X, Y).
ancestor2(X, Y):- parent(X, Z), ancestor2(Z, Y).

isList([]).
isList([_|_]).

% [1|[0|[]]] -> [1, 0]
% [1,[2,3,5], a,f,g,[[[[[[[[[[[[[[]]]]]]]]]]]]]]]

member1(X, [X|_]).
member1(X, [_|T]):- member1(X, T).

member3(X, [H|T]):- (H = X) ; member3(X, T).