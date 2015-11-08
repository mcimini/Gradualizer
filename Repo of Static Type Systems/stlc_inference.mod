module stlc_inference.

typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (absInf E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).

value (absInf E).
step (app (absInf E1) E2) (E1 E2).