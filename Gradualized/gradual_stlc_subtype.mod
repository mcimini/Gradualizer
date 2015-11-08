module gradual_stlc_subtype.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T3, subtype T1 T3.

value (abs T E).

step (app (abs T E1) E2) (E1 E2).


typeOfGr (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfGr x T1 => typeOfGr (E x) T2)).
typeOfGr (app E1 E2) T2 :- typeOfGr E1 PM1, matchArrow PM1 T1 T2, typeOfGr E2 T3, subtypeG T1 T3.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T3, subtype T1 T3.
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1.
typeOfCC (blame L) T.
typeOfCI (abs T1 E) (abs T1 (x\ (E' x))) (arrow T1 T2) :- (pi x\ (typeOfCI x x T1 => typeOfCI (E x) (E' x) T2)).
typeOfCI (app E1 E2) (app (cast E1' PM1 L (arrow New1 T2)) E2') T2 :- typeOfCI E1 E1' PM1, matchArrow PM1 T1 T2, typeOfCI E2 E2' T3, subtypeCI T1 T3 New1, flow T1 New1.
value (cast V T L (dyn)) :- value V.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
error (blame L).
step (cast V T L T) V :- value V.
step (cast E B L2 C) (cast V A L2 C) :- E = (cast V A L1 B), value V, flow A C.
step (cast E B L2 C) (blame L2) :- E = (cast V A L1 B), value V, not (flow A C).
step (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V E2) T2' L T2) :- value V.
matchInt (int).
matchInt (dyn).
matchArrow (arrow T1 T2) T1 T2.
matchArrow (dyn) (dyn) (dyn).
matchDyn (dyn).
subtypeG X1 X3 :- flow X1 X2, subtype X2 X3.
subtypeCI X1 X3 X2 :- flow X1 X2, subtype X2 X3.
