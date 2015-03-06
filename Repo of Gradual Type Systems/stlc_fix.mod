module stlc_fix.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (fix E) T :- typeOf E (arrow T T).

typeOfGr (abs O33 R) (arrow O33 O34) :- (pi x\ (typeOfGr x O33 => typeOfGr (R x) O34)).
typeOfGr (app E1 E2) O36 :- typeOfGr E1 PM35, matcharrow PM35 T1 O36, typeOfGr E2 O37, consistency O37 T1.
typeOfGr (fix E) T :- typeOfGr E PM38, matcharrow PM38 T O39, consistency O39 T.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (fix E) T :- typeOfCC E (arrow T T).
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O40 R) (abs O40 R') (arrow O40 O41) :- (pi x\ (typeOfCI x x O40 => typeOfCI (R x) (R' x) O41)).
typeOfCI (app E1 E2) (app (cast E1' PM42 L44 (arrow T1 O45)) (cast E2' O43 L46 T1)) O45 :- typeOfCI E1 E1' PM42, matcharrow PM42 T1 O45, typeOfCI E2 E2' O43, consistency O43 T1.
typeOfCI (fix E) (fix (cast E' PM47 L49 (arrow T T))) T :- typeOfCI E E' PM47, matcharrow PM47 T O48, consistency O48 T.


join2 X dyn X.
join2 dyn X X.
join2 X X X.

consistency X1 X2 :- join2 X1 X2 JoinX.
