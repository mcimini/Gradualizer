module stlc_ref.

typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (unit) (unitType).
typeOf (ref E) (refType T) :- typeOf E T.
typeOf (deref E) T :- typeOf E (refType T).
typeOf (assign E1 E2) (unitType) :- typeOf E1 (refType T), typeOf E2 T.

step (app (abs T E1) E2) M (E1 E2) M.

value (abs T E).
value (unit).
value (ref V) :- value V. 

typeOf (loc L) (refType T) :- typeOfRuntype L T.
step (ref V) M (loc L) (entry L V M) :- fresh L M, value V.
step (assign (loc L) V) M (unit) (entry L V M) :- value V.
step (deref (loc L)) M V M :- lookup L M V.

 
