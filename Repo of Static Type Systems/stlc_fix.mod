module stlc_fix.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (fix E) T :- typeOf E (arrow T T).

value (abs T E).

step (fix V) (app V (fix V)) :- value V.