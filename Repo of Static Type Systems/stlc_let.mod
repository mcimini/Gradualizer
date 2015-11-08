module stlc_let.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (let E1 E2) T2 :- typeOf E1 T1, (pi x\ (typeOf x T1 => typeOf (E2 x) T2)).

value (abs T E).

step (let V E) (E V) :- value V.