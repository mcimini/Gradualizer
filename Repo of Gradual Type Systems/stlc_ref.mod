module stlc_ref.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (ref E) (refType T) :- typeOf E T.
typeOf (deref E) T :- typeOf E (refType T).
typeOf (assign E1 E2) (unitType) :- typeOf E1 (refType T), typeOf E2 T.

typeOfGr (abs O69 R) (arrow O69 O70) :- (pi x\ (typeOfGr x O69 => typeOfGr (R x) O70)).
typeOfGr (app E1 E2) O72 :- typeOfGr E1 PM71, matcharrow PM71 T1 O72, typeOfGr E2 O73, consistency O73 T1.
typeOfGr (ref E) (refType O74) :- typeOfGr E O74.
typeOfGr (deref E) T :- typeOfGr E PM75, matchrefType PM75 T.
typeOfGr (assign E1 E2) (unitType) :- typeOfGr E1 PM76, matchrefType PM76 T, typeOfGr E2 O77, consistency O77 T.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (ref E) (refType T) :- typeOfCC E T.
typeOfCC (deref E) T :- typeOfCC E (refType T).
typeOfCC (assign E1 E2) (unitType) :- typeOfCC E1 (refType T), typeOfCC E2 T.
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O78 R) (abs O78 R') (arrow O78 O79) :- (pi x\ (typeOfCI x x O78 => typeOfCI (R x) (R' x) O79)).
typeOfCI (app E1 E2) (app (cast E1' PM80 L82 (arrow T1 O83)) (cast E2' O81 L84 T1)) O83 :- typeOfCI E1 E1' PM80, matcharrow PM80 T1 O83, typeOfCI E2 E2' O81, consistency O81 T1.
typeOfCI (ref E) (ref E') (refType O85) :- typeOfCI E E' O85.
typeOfCI (deref E) (deref (cast E' PM86 L87 (refType T))) T :- typeOfCI E E' PM86, matchrefType PM86 T.
typeOfCI (assign E1 E2) (assign (cast E1' PM88 L90 (refType T)) (cast E2' O89 L91 T)) (unitType) :- typeOfCI E1 E1' PM88, matchrefType PM88 T, typeOfCI E2 E2' O89, consistency O89 T.

matchrefType (refType X1) X1.
matchrefType (dyn) (dyn).


join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (refType X1) (refType Y1) (refType Z1) :- join2 X1 Y1 Z1.

consistency X1 X2 :- join2 X1 X2 JoinX.
