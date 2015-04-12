module stlc_ref.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (ref E) (refType T) :- typeOf E T.
typeOf (deref E) T :- typeOf E (refType T).
typeOf (assign E1 E2) (unitType) :- typeOf E1 (refType T), typeOf E2 T.

typeOfGr (abs O1 R) (arrow O1 O2) :- (pi x\ (typeOfGr x O1 => typeOfGr (R x) O2)).
typeOfGr (app E1 E2) O4 :- typeOfGr E1 PM3, matcharrow PM3 T1 O4, typeOfGr E2 O5, consistency O5 T1.
typeOfGr (ref E) (refType O6) :- typeOfGr E O6.
typeOfGr (deref E) T :- typeOfGr E PM7, matchrefType PM7 T.
typeOfGr (assign E1 E2) (unitType) :- typeOfGr E1 PM8, matchrefType PM8 T, typeOfGr E2 O9, consistency O9 T.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (ref E) (refType T) :- typeOfCC E T.
typeOfCC (deref E) T :- typeOfCC E (refType T).
typeOfCC (assign E1 E2) (unitType) :- typeOfCC E1 (refType T), typeOfCC E2 T.
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O10 R) (abs O10 R') (arrow O10 O11) :- (pi x\ (typeOfCI x x O10 => typeOfCI (R x) (R' x) O11)).
typeOfCI (app E1 E2) (app (cast E1' PM12 L14 (arrow T1 O15)) (cast E2' O13 L16 T1)) O15 :- typeOfCI E1 E1' PM12, matcharrow PM12 T1 O15, typeOfCI E2 E2' O13, consistency O13 T1.
typeOfCI (ref E) (ref E') (refType O17) :- typeOfCI E E' O17.
typeOfCI (deref E) (deref (cast E' PM18 L19 (refType T))) T :- typeOfCI E E' PM18, matchrefType PM18 T.
typeOfCI (assign E1 E2) (assign (cast E1' PM20 L22 (refType T)) (cast E2' O21 L23 T)) (unitType) :- typeOfCI E1 E1' PM20, matchrefType PM20 T, typeOfCI E2 E2' O21, consistency O21 T.

matcharrow (arrow X1 X2) X1 X2.
matcharrow (dyn) (dyn) (dyn).

matchrefType (refType X1) X1.
matchrefType (dyn) (dyn).


join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (refType X1) (refType Y1) (refType Z1) :- join2 X1 Y1 Z1.

consistency X1 X2 :- join2 X1 X2 JoinX.
