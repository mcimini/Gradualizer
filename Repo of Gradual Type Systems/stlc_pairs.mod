module stlc_pairs.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (pair E1 E2) (pairType T1 T2) :- typeOf E1 T1, typeOf E2 T2.
typeOf (fst E) T1 :- typeOf E (pairType T1 T2).
typeOf (snd E) T2 :- typeOf E (pairType T1 T2).

typeOfGr (abs O92 R) (arrow O92 O93) :- (pi x\ (typeOfGr x O92 => typeOfGr (R x) O93)).
typeOfGr (app E1 E2) O95 :- typeOfGr E1 PM94, matcharrow PM94 T1 O95, typeOfGr E2 O96, consistency O96 T1.
typeOfGr (pair E1 E2) (pairType O97 O98) :- typeOfGr E1 O97, typeOfGr E2 O98.
typeOfGr (fst E) O100 :- typeOfGr E PM99, matchpairType PM99 O100 O101.
typeOfGr (snd E) O103 :- typeOfGr E PM102, matchpairType PM102 O104 O103.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (pair E1 E2) (pairType T1 T2) :- typeOfCC E1 T1, typeOfCC E2 T2.
typeOfCC (fst E) T1 :- typeOfCC E (pairType T1 T2).
typeOfCC (snd E) T2 :- typeOfCC E (pairType T1 T2).
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O105 R) (abs O105 R') (arrow O105 O106) :- (pi x\ (typeOfCI x x O105 => typeOfCI (R x) (R' x) O106)).
typeOfCI (app E1 E2) (app (cast E1' PM107 L109 (arrow T1 O110)) (cast E2' O108 L111 T1)) O110 :- typeOfCI E1 E1' PM107, matcharrow PM107 T1 O110, typeOfCI E2 E2' O108, consistency O108 T1.
typeOfCI (pair E1 E2) (pair E1' E2') (pairType O112 O113) :- typeOfCI E1 E1' O112, typeOfCI E2 E2' O113.
typeOfCI (fst E) (fst (cast E' PM114 L115 (pairType O116 O117))) O116 :- typeOfCI E E' PM114, matchpairType PM114 O116 O117.
typeOfCI (snd E) (snd (cast E' PM118 L119 (pairType O120 O121))) O121 :- typeOfCI E E' PM118, matchpairType PM118 O120 O121.

matchpairType (pairType X1 X2) X1 X2.
matchpairType (dyn) (dyn) (dyn).


join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (pairType X1 X2) (pairType Y1 Y2) (pairType Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.

consistency X1 X2 :- join2 X1 X2 JoinX.
