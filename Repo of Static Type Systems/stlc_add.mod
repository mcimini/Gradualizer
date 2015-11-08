module stlc_add.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (add E1 E2) (int) :- typeOf E1 (int), typeOf E2 (int).
typeOf (zero) (int).
typeOf (succ E) (int) :- typeOf E (int).

value (abs T E).
value (succ E) :- value E.
value zero.