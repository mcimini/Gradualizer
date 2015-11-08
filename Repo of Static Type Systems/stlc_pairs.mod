module stlc_pairs.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (pair E1 E2) (pairType T1 T2) :- typeOf E1 T1, typeOf E2 T2.
typeOf (fst E) T1 :- typeOf E (pairType T1 T2).
typeOf (snd E) T2 :- typeOf E (pairType T1 T2).

value (abs T E).
value (pair V1 V2) :- value V1, value V2.

step (fst (pair E1 E2)) E1.
step (snd (pair E1 E2)) E2.