module stlc_sum.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (pair E1 E2) (pairType T1 T2) :- typeOf E1 T1, typeOf E2 T2.
typeOf (fst E) T1 :- typeOf E (pairType T1 T2).
typeOf (snd E) T2 :- typeOf E (pairType T1 T2).

typeOfGr (abs O1 R) (arrow O1 O2) :- (pi x\ (typeOfGr x O1 => typeOfGr (R x) O2)).
typeOfGr (app E1 E2) O4 :- typeOfGr E1 PM3, matcharrow PM3 T1 O4, typeOfGr E2 O5, consistency O5 T1.
typeOfGr (pair E1 E2) (pairType O6 O7) :- typeOfGr E1 O6, typeOfGr E2 O7.
typeOfGr (fst E) O9 :- typeOfGr E PM8, matchpairType PM8 O9 O10.
typeOfGr (snd E) O12 :- typeOfGr E PM11, matchpairType PM11 O13 O12.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (pair E1 E2) (pairType T1 T2) :- typeOfCC E1 T1, typeOfCC E2 T2.
typeOfCC (fst E) T1 :- typeOfCC E (pairType T1 T2).
typeOfCC (snd E) T2 :- typeOfCC E (pairType T1 T2).
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O14 R) (abs O14 R') (arrow O14 O15) :- (pi x\ (typeOfCI x x O14 => typeOfCI (R x) (R' x) O15)).
typeOfCI (app E1 E2) (app (cast E1' PM16 L18 (arrow T1 O19)) (cast E2' O17 L20 T1)) O19 :- typeOfCI E1 E1' PM16, matcharrow PM16 T1 O19, typeOfCI E2 E2' O17, consistency O17 T1.
typeOfCI (pair E1 E2) (pair E1' E2') (pairType O21 O22) :- typeOfCI E1 E1' O21, typeOfCI E2 E2' O22.
typeOfCI (fst E) (fst (cast E' PM23 L24 (pairType O25 O26))) O25 :- typeOfCI E E' PM23, matchpairType PM23 O25 O26.
typeOfCI (snd E) (snd (cast E' PM27 L28 (pairType O29 O30))) O30 :- typeOfCI E E' PM27, matchpairType PM27 O29 O30.

matcharrow (arrow X1 X2) X1 X2.
matcharrow (dyn) (dyn) (dyn).

matchpairType (pairType X1 X2) X1 X2.
matchpairType (dyn) (dyn) (dyn).


join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (pairType X1 X2) (pairType Y1 Y2) (pairType Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.

consistency X1 X2 :- join2 X1 X2 JoinX.
