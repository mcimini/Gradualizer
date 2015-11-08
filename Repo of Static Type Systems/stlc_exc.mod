module stlc_exc.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (raise T E) T :- typeOf E (excType).
typeOf (try E1 E2) T :- typeOf E1 T, typeOf E2 (arrow (excType) T).

step (try V E)  V :- value V.
step (try (raise T V) E)  (app E V) :- value V.

value (abs T E).
error (raise T E).
