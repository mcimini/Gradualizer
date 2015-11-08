module gradual_stlc_ref.


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

 


typeOfGr (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfGr x T1 => typeOfGr (E x) T2)).
typeOfGr (app E1 E2) T2 :- typeOfGr E1 PM1, matchArrow PM1 T1 T2, typeOfGr E2 New1, flow New1 T1.
typeOfGr (unit) (unitType).
typeOfGr (ref E) (refType T) :- typeOfGr E T.
typeOfGr (deref E) T :- typeOfGr E PM1, matchRefType PM1 T.
typeOfGr (assign E1 E2) (unitType) :- typeOfGr E1 PM1, matchRefType PM1 T, typeOfGr E2 New1, flow New1 T.
typeOfGr (loc L) (refType T) :- typeOfRuntype L T.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (unit) (unitType).
typeOfCC (ref E) (refType T) :- typeOfCC E T.
typeOfCC (deref E) T :- typeOfCC E (refType T).
typeOfCC (assign E1 E2) (unitType) :- typeOfCC E1 (refType T), typeOfCC E2 T.
typeOfCC (loc L) (refType T) :- typeOfRuntype L T.
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1.
typeOfCC (blame L) T.
typeOfCI (abs T1 E) (abs T1 (x\ (E' x))) (arrow T1 T2) :- (pi x\ (typeOfCI x x T1 => typeOfCI (E x) (E' x) T2)).
typeOfCI (app E1 E2) (app (cast E1' PM1 L (arrow T1 T2)) (cast E2' New1 L T1)) T2 :- typeOfCI E1 E1' PM1, matchArrow PM1 T1 T2, typeOfCI E2 E2' New1, flow New1 T1.
typeOfCI (unit) (unit) (unitType).
typeOfCI (ref E) (ref E') (refType T) :- typeOfCI E E' T.
typeOfCI (deref E) (deref (cast E' PM1 L (refType T))) T :- typeOfCI E E' PM1, matchRefType PM1 T.
typeOfCI (assign E1 E2) (assign (cast E1' PM1 L (refType T)) (cast E2' New1 L T)) (unitType) :- typeOfCI E1 E1' PM1, matchRefType PM1 T, typeOfCI E2 E2' New1, flow New1 T.
typeOfCI (loc L) (loc L') (refType T) :- typeOfRuntype L T.
value (cast V T L (dyn)) :- value V.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (refType T1) L (refType T1')) :- value V.
error (blame L).
step' (cast V T L T) V :- value V.
step' (cast E B L2 C) (cast V A L2 C) :- E = (cast V A L1 B), value V, flow A C.
step' (cast E B L2 C) (blame L2) :- E = (cast V A L1 B), value V, not (flow A C).
step' (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V.
step' (deref (cast V (refType T') L (refType T))) (cast (deref V) T' L T) :- value V.
step' (assign (cast V (refType T') L (refType T)) E2) (assign V (cast E2 T L T')) :- value V.
step E X1 E' X1 :- step' E E'.
matchInt (int).
matchInt (dyn).
matchArrow (arrow T1 T2) T1 T2.
matchArrow (dyn) (dyn) (dyn).
matchRefType (refType T1) T1.
matchRefType (dyn) (dyn).
matchUnitType (unitType).
matchUnitType (dyn).
matchDyn (dyn).
flow X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 X X X.
