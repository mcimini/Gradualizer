module stlc_rec.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (rec3 F1 E1 F2 E2 F3 E3) (rec3Type F1 T1 F2 T2 F3 T3) :- typeOf E1 T1, typeOf E2 T2, typeOf E3 T3.
typeOf (proj1 E F) T :- typeOf E (rec3Type F T F2 T2 F3 T3).
typeOf (proj2 E F) T :- typeOf E (rec3Type F1 T1 F T F3 T3).
typeOf (proj3 E F) T :- typeOf E (rec3Type F1 T1 F2 T2 F T).

typeOfGr (abs O1 R) (arrow O1 O2) :- (pi x\ (typeOfGr x O1 => typeOfGr (R x) O2)).
typeOfGr (app E1 E2) O4 :- typeOfGr E1 PM3, matcharrow PM3 T1 O4, typeOfGr E2 O5, consistency O5 T1.
typeOfGr (rec3 F1 E1 F2 E2 F3 E3) (rec3Type F1 O6 F2 O7 F3 O8) :- typeOfGr E1 O6, typeOfGr E2 O7, typeOfGr E3 O8.
typeOfGr (proj1 E F) O10 :- typeOfGr E PM9, matchrec3Type PM9 F O10 F2 O11 F3 O12.
typeOfGr (proj2 E F) O14 :- typeOfGr E PM13, matchrec3Type PM13 F1 O15 F O14 F3 O16.
typeOfGr (proj3 E F) O18 :- typeOfGr E PM17, matchrec3Type PM17 F1 O19 F2 O20 F O18.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (rec3 F1 E1 F2 E2 F3 E3) (rec3Type F1 T1 F2 T2 F3 T3) :- typeOfCC E1 T1, typeOfCC E2 T2, typeOfCC E3 T3.
typeOfCC (proj1 E F) T :- typeOfCC E (rec3Type F T F2 T2 F3 T3).
typeOfCC (proj2 E F) T :- typeOfCC E (rec3Type F1 T1 F T F3 T3).
typeOfCC (proj3 E F) T :- typeOfCC E (rec3Type F1 T1 F2 T2 F T).
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O21 R) (abs O21 R') (arrow O21 O22) :- (pi x\ (typeOfCI x x O21 => typeOfCI (R x) (R' x) O22)).
typeOfCI (app E1 E2) (app (cast E1' PM23 L25 (arrow T1 O26)) (cast E2' O24 L27 T1)) O26 :- typeOfCI E1 E1' PM23, matcharrow PM23 T1 O26, typeOfCI E2 E2' O24, consistency O24 T1.
typeOfCI (rec3 F1 E1 F2 E2 F3 E3) (rec3 F1 E1' F2 E2' F3 E3') (rec3Type F1 O28 F2 O29 F3 O30) :- typeOfCI E1 E1' O28, typeOfCI E2 E2' O29, typeOfCI E3 E3' O30.
typeOfCI (proj1 E F) (proj1 (cast E' PM31 L32 (rec3Type F O33 F2 O34 F3 O35)) F) O33 :- typeOfCI E E' PM31, matchrec3Type PM31 F O33 F2 O34 F3 O35.
typeOfCI (proj2 E F) (proj2 (cast E' PM36 L37 (rec3Type F1 O38 F O39 F3 O40)) F) O39 :- typeOfCI E E' PM36, matchrec3Type PM36 F1 O38 F O39 F3 O40.
typeOfCI (proj3 E F) (proj3 (cast E' PM41 L42 (rec3Type F1 O43 F2 O44 F O45)) F) O45 :- typeOfCI E E' PM41, matchrec3Type PM41 F1 O43 F2 O44 F O45.

matcharrow (arrow X1 X2) X1 X2.
matcharrow (dyn) (dyn) (dyn).

matchrec3Type (rec3Type X1 X2 X3 X4 X5 X6) X1 X2 X3 X4 X5 X6.
matchrec3Type (dyn) X46 (dyn) X47 (dyn) X48 (dyn).


join6 X1 X2 X3 X4 X5 X6 Xjoin :- join2 X1 X2 Xtmp, join5 Xtmp X3 X4 X5 X6 Xjoin.
join5 X1 X2 X3 X4 X5 Xjoin :- join2 X1 X2 Xtmp, join4 Xtmp X3 X4 X5 Xjoin.
join4 X1 X2 X3 X4 Xjoin :- join2 X1 X2 Xtmp, join3 Xtmp X3 X4 Xjoin.
join3 X1 X2 X3 Xjoin :- join2 X1 X2 Xtmp, join2 Xtmp X3 Xjoin.
join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (rec3Type X1 X2 X3 X4 X5 X6) (rec3Type Y1 Y2 Y3 Y4 Y5 Y6) (rec3Type Z1 Z2 Z3 Z4 Z5 Z6) :- join2 X2 Y2 Z2, join2 X4 Y4 Z4, join2 X6 Y6 Z6.

consistency X1 X2 :- join2 X1 X2 JoinX.
