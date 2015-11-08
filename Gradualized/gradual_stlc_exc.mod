module gradual_stlc_exc.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (raise T E) T :- typeOf E (excType).
typeOf (try E1 E2) T :- typeOf E1 T, typeOf E2 (arrow (excType) T).

step (try V E)  V :- value V.
step (try (raise T V) E)  (app E V) :- value V.

value (abs T E).
error (raise T E).


typeOfGr (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfGr x T1 => typeOfGr (E x) T2)).
typeOfGr (app E1 E2) T2 :- typeOfGr E1 PM1, matchArrow PM1 T1 T2, typeOfGr E2 New1, flow New1 T1.
typeOfGr (raise T E) T :- typeOfGr E PM1, matchExcType PM1.
typeOfGr (try E1 E2) JoinT :- typeOfGr E1 New1, flow New1 JoinT, typeOfGr E2 PM2, matchArrow PM2 PM1 New2, flow New2 JoinT, matchExcType PM1, join2 New2 New1 JoinT.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (raise T E) T :- typeOfCC E (excType).
typeOfCC (try E1 E2) T :- typeOfCC E1 T, typeOfCC E2 (arrow (excType) T).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1.
typeOfCC (blame L) T.
typeOfCI (abs T1 E) (abs T1 (x\ (E' x))) (arrow T1 T2) :- (pi x\ (typeOfCI x x T1 => typeOfCI (E x) (E' x) T2)).
typeOfCI (app E1 E2) (app (cast E1' PM1 L (arrow T1 T2)) (cast E2' New1 L T1)) T2 :- typeOfCI E1 E1' PM1, matchArrow PM1 T1 T2, typeOfCI E2 E2' New1, flow New1 T1.
typeOfCI (raise T E) (raise T (cast E' PM1 L (excType))) T :- typeOfCI E E' PM1, matchExcType PM1.
typeOfCI (try E1 E2) (try (cast E1' New1 L JoinT) (cast E2' PM2 L (arrow (excType) JoinT))) JoinT :- typeOfCI E1 E1' New1, flow New1 JoinT, typeOfCI E2 E2' PM2, matchArrow PM2 PM1 New2, flow New2 JoinT, matchExcType PM1, join2 New2 New1 JoinT.
value (cast V T L (dyn)) :- value V.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
error (blame L).
step (cast V T L T) V :- value V.
step (cast E B L2 C) (cast V A L2 C) :- E = (cast V A L1 B), value V, flow A C.
step (cast E B L2 C) (blame L2) :- E = (cast V A L1 B), value V, not (flow A C).
step (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V.
matchInt (int).
matchInt (dyn).
matchArrow (arrow T1 T2) T1 T2.
matchArrow (dyn) (dyn) (dyn).
matchExcType (excType).
matchExcType (dyn).
matchDyn (dyn).
flow X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 X X X.
join2 (int) (int) (int).
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (excType) (excType) (excType).
join2 (dyn) (dyn) (dyn).
