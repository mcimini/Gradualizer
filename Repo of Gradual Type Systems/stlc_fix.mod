module stlc_fix.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (fix E) T :- typeOf E (arrow T T).

typeOfGr (abs O1 R) (arrow O1 O2) :- (pi x\ (typeOfGr x O1 => typeOfGr (R x) O2)).
typeOfGr (app E1 E2) O4 :- typeOfGr E1 PM3, matcharrow PM3 T1 O4, typeOfGr E2 O5, consistency O5 T1.
typeOfGr (fix E) T :- typeOfGr E PM6, matcharrow PM6 T O7, consistency O7 T.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (fix E) T :- typeOfCC E (arrow T T).
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O8 R) (abs O8 R') (arrow O8 O9) :- (pi x\ (typeOfCI x x O8 => typeOfCI (R x) (R' x) O9)).
typeOfCI (app E1 E2) (app (cast E1' PM10 L12 (arrow T1 O13)) (cast E2' O11 L14 T1)) O13 :- typeOfCI E1 E1' PM10, matcharrow PM10 T1 O13, typeOfCI E2 E2' O11, consistency O11 T1.
typeOfCI (fix E) (fix (cast E' PM15 L17 (arrow T T))) T :- typeOfCI E E' PM15, matcharrow PM15 T O16, consistency O16 T.

matcharrow (arrow X1 X2) X1 X2.
matcharrow (dyn) (dyn) (dyn).


join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.

consistency X1 X2 :- join2 X1 X2 JoinX.
