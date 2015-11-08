module stlc_tuples.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (tuple E1 E2 E3 E4) (tupleType T1 T2 T3 T4) :- typeOf E1 T1, typeOf E2 T2, typeOf E3 T3, typeOf E4 T4.
typeOf (select1 E) T1 :- typeOf E (tupleType T1 T2 T3 T4).
typeOf (select2 E) T2 :- typeOf E (tupleType T1 T2 T3 T4).
typeOf (select3 E) T3 :- typeOf E (tupleType T1 T2 T3 T4).
typeOf (select4 E) T4 :- typeOf E (tupleType T1 T2 T3 T4).

value (abs T E).
value (tuple V1 V2 V3 V4) :- value V1, value V2, value V3, value V4. 

step (select1 (tuple E1 E2 E3 E4)) E1. 
step (select2 (tuple E1 E2 E3 E4)) E2. 
step (select3 (tuple E1 E2 E3 E4)) E3. 
step (select4 (tuple E1 E2 E3 E4)) E4.
