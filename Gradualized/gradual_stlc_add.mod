module gradual_stlc_add.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (add E1 E2) (int) :- typeOf E1 (int), typeOf E2 (int).
typeOf (zero) (int).
typeOf (succ E) (int) :- typeOf E (int).

value (abs T E).
value (succ E) :- value E.
value zero.


typeOfGr (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfGr x T1 => typeOfGr (E x) T2)).
typeOfGr (app E1 E2) T2 :- typeOfGr E1 PM1, matchArrow PM1 T1 T2, typeOfGr E2 New1, flow New1 T1.
typeOfGr (add E1 E2) (int) :- typeOfGr E1 PM1, matchInt PM1, typeOfGr E2 PM2, matchInt PM2.
typeOfGr (zero) (int).
typeOfGr (succ E) (int) :- typeOfGr E PM1, matchInt PM1.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (add E1 E2) (int) :- typeOfCC E1 (int), typeOfCC E2 (int).
typeOfCC (zero) (int).
typeOfCC (succ E) (int) :- typeOfCC E (int).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1.
typeOfCC (blame L) T.
typeOfCI (abs T1 E) (abs T1 (x\ (E' x))) (arrow T1 T2) :- (pi x\ (typeOfCI x x T1 => typeOfCI (E x) (E' x) T2)).
typeOfCI (app E1 E2) (app (cast E1' PM1 L (arrow T1 T2)) (cast E2' New1 L T1)) T2 :- typeOfCI E1 E1' PM1, matchArrow PM1 T1 T2, typeOfCI E2 E2' New1, flow New1 T1.
typeOfCI (add E1 E2) (add (cast E1' PM1 L (int)) (cast E2' PM2 L (int))) (int) :- typeOfCI E1 E1' PM1, matchInt PM1, typeOfCI E2 E2' PM2, matchInt PM2.
typeOfCI (zero) (zero) (int).
typeOfCI (succ E) (succ (cast E' PM1 L (int))) (int) :- typeOfCI E E' PM1, matchInt PM1.
value (cast V T L (dyn)) :- value V.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
error (blame L).
step (cast V T L T) V :- value V.
step (cast E B L2 C) (cast V A L2 C) :- E = (cast V A L1 B), value V, flow A C.
step (cast E B L2 C) (blame L2) :- E = (cast V A L1 B), value V, not (flow A C).
step (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V.
matchInt (int).
matchInt (dyn).
matchArrow (arrow T1 T2) T1 T2.
matchArrow (dyn) (dyn) (dyn).
matchDyn (dyn).
flow X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 X X X.
