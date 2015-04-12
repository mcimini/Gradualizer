module stlc_lists.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (isnil T E) (bool) :- typeOf E (list T).
typeOf (cons T E1 E2) (list T) :- typeOf E1 T, typeOf E2 (list T).
typeOf (head T E) T :- typeOf E (list T).
typeOf (tail T E) T :- typeOf E (list T).

typeOfGr (abs O1 R) (arrow O1 O2) :- (pi x\ (typeOfGr x O1 => typeOfGr (R x) O2)).
typeOfGr (app E1 E2) O4 :- typeOfGr E1 PM3, matcharrow PM3 T1 O4, typeOfGr E2 O5, consistency O5 T1.
typeOfGr (isnil O7 E) (bool) :- typeOfGr E PM6, matchlist PM6 O8, join2 O8 O7 JoinT.
typeOfGr (cons O10 E1 E2) (list JoinT) :- typeOfGr E1 O11, typeOfGr E2 PM9, matchlist PM9 O12, join3 O11 O12 O10 JoinT.
typeOfGr (head O14 E) JoinT :- typeOfGr E PM13, matchlist PM13 O15, join2 O15 O14 JoinT.
typeOfGr (tail O17 E) JoinT :- typeOfGr E PM16, matchlist PM16 O18, join2 O18 O17 JoinT.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (isnil T E) (bool) :- typeOfCC E (list T).
typeOfCC (cons T E1 E2) (list T) :- typeOfCC E1 T, typeOfCC E2 (list T).
typeOfCC (head T E) T :- typeOfCC E (list T).
typeOfCC (tail T E) T :- typeOfCC E (list T).
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O19 R) (abs O19 R') (arrow O19 O20) :- (pi x\ (typeOfCI x x O19 => typeOfCI (R x) (R' x) O20)).
typeOfCI (app E1 E2) (app (cast E1' PM21 L23 (arrow T1 O24)) (cast E2' O22 L25 T1)) O24 :- typeOfCI E1 E1' PM21, matcharrow PM21 T1 O24, typeOfCI E2 E2' O22, consistency O22 T1.
typeOfCI (isnil O27 E) (isnil JoinT (cast E' PM26 L29 (list JoinT))) (bool) :- typeOfCI E E' PM26, matchlist PM26 O28, join2 O28 O27 JoinT.
typeOfCI (cons O31 E1 E2) (cons JoinT (cast E1' O33 L34 JoinT) (cast E2' PM30 L35 (list JoinT))) (list JoinT) :- typeOfCI E1 E1' O33, typeOfCI E2 E2' PM30, matchlist PM30 O32, join3 O33 O32 O31 JoinT.
typeOfCI (head O37 E) (head JoinT (cast E' PM36 L39 (list JoinT))) JoinT :- typeOfCI E E' PM36, matchlist PM36 O38, join2 O38 O37 JoinT.
typeOfCI (tail O41 E) (tail JoinT (cast E' PM40 L43 (list JoinT))) JoinT :- typeOfCI E E' PM40, matchlist PM40 O42, join2 O42 O41 JoinT.

matcharrow (arrow X1 X2) X1 X2.
matcharrow (dyn) (dyn) (dyn).

matchlist (list X1) X1.
matchlist (dyn) (dyn).


join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (list X1) (list Y1) (list Z1) :- join2 X1 Y1 Z1.

consistency X1 X2 :- join2 X1 X2 JoinX.
