module stlc_exc.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (raise T E) T :- typeOf E (excType).
typeOf (try E1 E2) T :- typeOf E1 T, typeOf E2 (excTypeFun T).

typeOfGr (abs O1 R) (arrow O1 O2) :- (pi x\ (typeOfGr x O1 => typeOfGr (R x) O2)).
typeOfGr (app E1 E2) O4 :- typeOfGr E1 PM3, matcharrow PM3 T1 O4, typeOfGr E2 O5, consistency O5 T1.
typeOfGr (raise O7 E) O7 :- typeOfGr E PM6, matchexcType PM6.
typeOfGr (try E1 E2) JoinT :- typeOfGr E1 O9, typeOfGr E2 PM8, matchexcTypeFun PM8 O10, join2 O9 O10 JoinT.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (raise T E) T :- typeOfCC E (excType).
typeOfCC (try E1 E2) T :- typeOfCC E1 T, typeOfCC E2 (excTypeFun T).
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O11 R) (abs O11 R') (arrow O11 O12) :- (pi x\ (typeOfCI x x O11 => typeOfCI (R x) (R' x) O12)).
typeOfCI (app E1 E2) (app (cast E1' PM13 L15 (arrow T1 O16)) (cast E2' O14 L17 T1)) O16 :- typeOfCI E1 E1' PM13, matcharrow PM13 T1 O16, typeOfCI E2 E2' O14, consistency O14 T1.
typeOfCI (raise O19 E) (raise O19 (cast E' PM18 L20 (excType))) O19 :- typeOfCI E E' PM18, matchexcType PM18.
typeOfCI (try E1 E2) (try (cast E1' O23 L24 JoinT) (cast E2' PM21 L25 (excTypeFun JoinT))) JoinT :- typeOfCI E1 E1' O23, typeOfCI E2 E2' PM21, matchexcTypeFun PM21 O22, join2 O23 O22 JoinT.

matchexcType (excType).
matchexcType (dyn).


join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (excType) (excType) (excType).

consistency X1 X2 :- join2 X1 X2 JoinX.
