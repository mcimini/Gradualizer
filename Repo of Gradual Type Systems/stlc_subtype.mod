module stlc_subtype.

accumulate subtyping.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T3, subtype T3 T1.

typeOfGr (abs O1 R) (arrow O1 O2) :- (pi x\ (typeOfGr x O1 => typeOfGr (R x) O2)).
typeOfGr (app E1 E2) O4 :- typeOfGr E1 PM3, matcharrow PM3 T1 O4, typeOfGr E2 O5, subtypeGr O5 O6, consistency O6 T1.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T3, subtypeCC T3 T1.
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O7 R) (abs O7 R') (arrow O7 O8) :- (pi x\ (typeOfCI x x O7 => typeOfCI (R x) (R' x) O8)).
typeOfCI (app E1 E2) (app (cast E1' PM9 L12 (arrow T1 O13)) E2') O13 :- typeOfCI E1 E1' PM9, matcharrow PM9 T1 O13, typeOfCI E2 E2' O10, subtypeCI O10 O11, consistency O11 T1.

matcharrow (arrow X1 X2) X1 X2.
matcharrow (dyn) (dyn) (dyn).


join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.

consistency X1 X2 :- join2 X1 X2 JoinX.
