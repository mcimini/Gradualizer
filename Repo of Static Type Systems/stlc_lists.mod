module stlc_lists.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (tt) (bool).
typeOf (ff) (bool).
typeOf (emptyList T) (list T).
typeOf (isnil T E) (bool) :- typeOf E (list T).
typeOf (cons T E1 E2) (list T) :- typeOf E1 T, typeOf E2 (list T).
typeOf (head T E) T :- typeOf E (list T).
typeOf (tail T E) T :- typeOf E (list T).

value (abs T E).
value (tt).
value (ff).
value (emptyList T).
value (cons T V1 V2) :- value V1, value V2.

step (isnil T (emptyList T)) (tt).
step (isnil T (cons T E1 E2)) (ff).
step (head T (cons T E1 E2)) E1.
step (tail T (cons T E1 E2)) E2.
