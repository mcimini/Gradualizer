module gradual_stlc_pairs.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (pair E1 E2) (pairType T1 T2) :- typeOf E1 T1, typeOf E2 T2.
typeOf (fst E) T1 :- typeOf E (pairType T1 T2).
typeOf (snd E) T2 :- typeOf E (pairType T1 T2).

value (abs T E).
value (pair V1 V2) :- value V1, value V2.

step (fst (pair E1 E2)) E1.
step (snd (pair E1 E2)) E2.


typeOfGr (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfGr x T1 => typeOfGr (E x) T2)).
typeOfGr (app E1 E2) T2 :- typeOfGr E1 PM1, matchArrow PM1 T1 T2, typeOfGr E2 New1, flow New1 T1.
typeOfGr (pair E1 E2) (pairType T1 T2) :- typeOfGr E1 T1, typeOfGr E2 T2.
typeOfGr (fst E) T1 :- typeOfGr E PM1, matchPairType PM1 T1 T2.
typeOfGr (snd E) T2 :- typeOfGr E PM1, matchPairType PM1 T1 T2.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (pair E1 E2) (pairType T1 T2) :- typeOfCC E1 T1, typeOfCC E2 T2.
typeOfCC (fst E) T1 :- typeOfCC E (pairType T1 T2).
typeOfCC (snd E) T2 :- typeOfCC E (pairType T1 T2).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1.
typeOfCC (blame L) T.
typeOfCI (abs T1 E) (abs T1 (x\ (E' x))) (arrow T1 T2) :- (pi x\ (typeOfCI x x T1 => typeOfCI (E x) (E' x) T2)).
typeOfCI (app E1 E2) (app (cast E1' PM1 L (arrow T1 T2)) (cast E2' New1 L T1)) T2 :- typeOfCI E1 E1' PM1, matchArrow PM1 T1 T2, typeOfCI E2 E2' New1, flow New1 T1.
typeOfCI (pair E1 E2) (pair E1' E2') (pairType T1 T2) :- typeOfCI E1 E1' T1, typeOfCI E2 E2' T2.
typeOfCI (fst E) (fst (cast E' PM1 L (pairType T1 T2))) T1 :- typeOfCI E E' PM1, matchPairType PM1 T1 T2.
typeOfCI (snd E) (snd (cast E' PM1 L (pairType T1 T2))) T2 :- typeOfCI E E' PM1, matchPairType PM1 T1 T2.
value (cast V T L (dyn)) :- value V.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (pairType T1 T2) L (pairType T1' T2')) :- value V.
error (blame L).
step (cast V T L T) V :- value V.
step (cast E B L2 C) (cast V A L2 C) :- E = (cast V A L1 B), value V, flow A C.
step (cast E B L2 C) (blame L2) :- E = (cast V A L1 B), value V, not (flow A C).
step (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V.
step (fst (cast V (pairType T1' T2') L (pairType T1 T2))) (cast (fst V) T1' L T1) :- value V.
step (snd (cast V (pairType T1' T2') L (pairType T1 T2))) (cast (snd V) T2' L T2) :- value V.
matchInt (int).
matchInt (dyn).
matchArrow (arrow T1 T2) T1 T2.
matchArrow (dyn) (dyn) (dyn).
matchPairType (pairType T1 T2) T1 T2.
matchPairType (dyn) (dyn) (dyn).
matchDyn (dyn).
flow X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 X X X.
