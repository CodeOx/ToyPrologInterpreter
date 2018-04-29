edge(a,b).
edge(b,c).
path(X,X).
path(X,Y) :- edge(X,Z),path(Z,Y).


s(X,Y) :- q(X,Y).
s(a,a).
 
q(X,Y) :- i(X),!,j(Y).
 
i(b).
i(c).
j(b).
j(c).
j(d).