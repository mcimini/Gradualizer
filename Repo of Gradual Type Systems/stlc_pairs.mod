module stlc_pairs.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (pair E1 E2) (pairType T1 T2) :- typeOf E1 T1, typeOf E2 T2.
typeOf (fst E) T1 :- typeOf E (pairType T1 T2).
typeOf (snd E) T2 :- typeOf E (pairType T1 T2).

typeOfGr (abs O81 R) (arrow O81 O82) :- (pi x\ (typeOfGr x O81 => typeOfGr (R x) O82)).
typeOfGr (app E1 E2) O84 :- typeOfGr E1 PM83, matcharrow PM83 T1 O84, typeOfGr E2 O85, consistency O85 T1.
typeOfGr (pair E1 E2) (pairType O86 O87) :- typeOfGr E1 O86, typeOfGr E2 O87.
typeOfGr (fst E) O89 :- typeOfGr E PM88, matchpairType PM88 O89 O90.
typeOfGr (snd E) O92 :- typeOfGr E PM91, matchpairType PM91 O93 O92.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (pair E1 E2) (pairType T1 T2) :- typeOfCC E1 T1, typeOfCC E2 T2.
typeOfCC (fst E) T1 :- typeOfCC E (pairType T1 T2).
typeOfCC (snd E) T2 :- typeOfCC E (pairType T1 T2).
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O94 R) (abs O94 R') (arrow O94 O95) :- (pi x\ (typeOfCI x x O94 => typeOfCI (R x) (R' x) O95)).
typeOfCI (app E1 E2) (app (cast E1' PM96 L98 (arrow T1 O99)) (cast E2' O97 L100 T1)) O99 :- typeOfCI E1 E1' PM96, matcharrow PM96 T1 O99, typeOfCI E2 E2' O97, consistency O97 T1.
typeOfCI (pair E1 E2) (pair E1' E2') (pairType O101 O102) :- typeOfCI E1 E1' O101, typeOfCI E2 E2' O102.
typeOfCI (fst E) (fst (cast E' PM103 L104 (pairType O105 O106))) O105 :- typeOfCI E E' PM103, matchpairType PM103 O105 O106.
typeOfCI (snd E) (snd (cast E' PM107 L108 (pairType O109 O110))) O110 :- typeOfCI E E' PM107, matchpairType PM107 O109 O110.

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
