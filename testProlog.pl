s(X,Y) :- q(X,Y).
s(0,0).
 
q(X,Y) :- i(X),!,j(Y).
 
i(1).
i(2).
j(1).
j(2).
j(3).

/*p(X):- a(X).
p(X):- b(X),c(X),!,d(X),e(X).
p(X):- f(X).
a(1).
b(1). b(2).
c(1). c(2).
d(2).
e(2).
f(3).*/

enjoys(v,X) :- big_kahuna_burger(X),!,fail.
enjoys(v,X) :- burger(X).
 
burger(X) :- big_mac(X).
burger(X) :- big_kahuna_burger(X).
burger(X) :- whopper(X).
 
big_mac(a).
big_kahuna_burger(b).
big_mac(c).
whopper(d).

a(X) :- b(X),c(X),!,fail.
a(X) :- d(X).

b(1).
b(4).
c(1).
c(3).

d(4).
d(1).