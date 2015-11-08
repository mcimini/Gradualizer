module gradual_stlc_tuples.


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


typeOfGr (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfGr x T1 => typeOfGr (E x) T2)).
typeOfGr (app E1 E2) T2 :- typeOfGr E1 PM1, matchArrow PM1 T1 T2, typeOfGr E2 New1, flow New1 T1.
typeOfGr (tuple E1 E2 E3 E4) (tupleType T1 T2 T3 T4) :- typeOfGr E1 T1, typeOfGr E2 T2, typeOfGr E3 T3, typeOfGr E4 T4.
typeOfGr (select1 E) T1 :- typeOfGr E PM1, matchTupleType PM1 T1 T2 T3 T4.
typeOfGr (select2 E) T2 :- typeOfGr E PM1, matchTupleType PM1 T1 T2 T3 T4.
typeOfGr (select3 E) T3 :- typeOfGr E PM1, matchTupleType PM1 T1 T2 T3 T4.
typeOfGr (select4 E) T4 :- typeOfGr E PM1, matchTupleType PM1 T1 T2 T3 T4.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (tuple E1 E2 E3 E4) (tupleType T1 T2 T3 T4) :- typeOfCC E1 T1, typeOfCC E2 T2, typeOfCC E3 T3, typeOfCC E4 T4.
typeOfCC (select1 E) T1 :- typeOfCC E (tupleType T1 T2 T3 T4).
typeOfCC (select2 E) T2 :- typeOfCC E (tupleType T1 T2 T3 T4).
typeOfCC (select3 E) T3 :- typeOfCC E (tupleType T1 T2 T3 T4).
typeOfCC (select4 E) T4 :- typeOfCC E (tupleType T1 T2 T3 T4).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1.
typeOfCC (blame L) T.
typeOfCI (abs T1 E) (abs T1 (x\ (E' x))) (arrow T1 T2) :- (pi x\ (typeOfCI x x T1 => typeOfCI (E x) (E' x) T2)).
typeOfCI (app E1 E2) (app (cast E1' PM1 L (arrow T1 T2)) (cast E2' New1 L T1)) T2 :- typeOfCI E1 E1' PM1, matchArrow PM1 T1 T2, typeOfCI E2 E2' New1, flow New1 T1.
typeOfCI (tuple E1 E2 E3 E4) (tuple E1' E2' E3' E4') (tupleType T1 T2 T3 T4) :- typeOfCI E1 E1' T1, typeOfCI E2 E2' T2, typeOfCI E3 E3' T3, typeOfCI E4 E4' T4.
typeOfCI (select1 E) (select1 (cast E' PM1 L (tupleType T1 T2 T3 T4))) T1 :- typeOfCI E E' PM1, matchTupleType PM1 T1 T2 T3 T4.
typeOfCI (select2 E) (select2 (cast E' PM1 L (tupleType T1 T2 T3 T4))) T2 :- typeOfCI E E' PM1, matchTupleType PM1 T1 T2 T3 T4.
typeOfCI (select3 E) (select3 (cast E' PM1 L (tupleType T1 T2 T3 T4))) T3 :- typeOfCI E E' PM1, matchTupleType PM1 T1 T2 T3 T4.
typeOfCI (select4 E) (select4 (cast E' PM1 L (tupleType T1 T2 T3 T4))) T4 :- typeOfCI E E' PM1, matchTupleType PM1 T1 T2 T3 T4.
value (cast V T L (dyn)) :- value V.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (tupleType T1 T2 T3 T4) L (tupleType T1' T2' T3' T4')) :- value V.
error (blame L).
step (cast V T L T) V :- value V.
step (cast E B L2 C) (cast V A L2 C) :- E = (cast V A L1 B), value V, flow A C.
step (cast E B L2 C) (blame L2) :- E = (cast V A L1 B), value V, not (flow A C).
step (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V.
step (select2 (cast V (tupleType T1' T2' T3' T4') L (tupleType T1 T2 T3 T4))) (cast (select2 V) T2' L T2) :- value V.
step (select3 (cast V (tupleType T1' T2' T3' T4') L (tupleType T1 T2 T3 T4))) (cast (select3 V) T3' L T3) :- value V.
step (select4 (cast V (tupleType T1' T2' T3' T4') L (tupleType T1 T2 T3 T4))) (cast (select4 V) T4' L T4) :- value V.
step (select1 (cast V (tupleType T1' T2' T3' T4') L (tupleType T1 T2 T3 T4))) (cast (select1 V) T1' L T1) :- value V.
matchInt (int).
matchInt (dyn).
matchArrow (arrow T1 T2) T1 T2.
matchArrow (dyn) (dyn) (dyn).
matchTupleType (tupleType T1 T2 T3 T4) T1 T2 T3 T4.
matchTupleType (dyn) (dyn) (dyn) (dyn) (dyn).
matchDyn (dyn).
flow X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 X X X.
