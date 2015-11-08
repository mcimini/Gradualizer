module gradual_stlc_lists.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (tt) (bool).
typeOf (ff) (bool).
typeOf (emptyList T) (list T).
typeOf (isnil T E) (bool) :- typeOf E (list T).
typeOf (cons T E1 E2) (list T) :- typeOf E1 T, typeOf E2 (list T).
typeOf (head T E) T :- typeOf E (list T).
typeOf (tail T E) T :- typeOf E (list T).

value (abs T E).
value (tt).
value (ff).
value (emptyList T).
value (cons T V1 V2) :- value V1, value V2.

step (isnil T (emptyList T)) (tt).
step (isnil T (cons T E1 E2)) (ff).
step (head T (cons T E1 E2)) E1.
step (tail T (cons T E1 E2)) E2.


typeOfGr (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfGr x T1 => typeOfGr (E x) T2)).
typeOfGr (app E1 E2) T2 :- typeOfGr E1 PM1, matchArrow PM1 T1 T2, typeOfGr E2 New1, flow New1 T1.
typeOfGr (tt) (bool).
typeOfGr (ff) (bool).
typeOfGr (emptyList T) (list T).
typeOfGr (isnil T E) (bool) :- typeOfGr E PM1, matchList PM1 New1, flow New1 T.
typeOfGr (cons T E1 E2) (list T) :- typeOfGr E1 New1, flow New1 T, typeOfGr E2 PM1, matchList PM1 New2, flow New2 T.
typeOfGr (head T E) T :- typeOfGr E PM1, matchList PM1 New1, flow New1 T.
typeOfGr (tail T E) T :- typeOfGr E PM1, matchList PM1 New1, flow New1 T.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (tt) (bool).
typeOfCC (ff) (bool).
typeOfCC (emptyList T) (list T).
typeOfCC (isnil T E) (bool) :- typeOfCC E (list T).
typeOfCC (cons T E1 E2) (list T) :- typeOfCC E1 T, typeOfCC E2 (list T).
typeOfCC (head T E) T :- typeOfCC E (list T).
typeOfCC (tail T E) T :- typeOfCC E (list T).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1.
typeOfCC (blame L) T.
typeOfCI (abs T1 E) (abs T1 (x\ (E' x))) (arrow T1 T2) :- (pi x\ (typeOfCI x x T1 => typeOfCI (E x) (E' x) T2)).
typeOfCI (app E1 E2) (app (cast E1' PM1 L (arrow T1 T2)) (cast E2' New1 L T1)) T2 :- typeOfCI E1 E1' PM1, matchArrow PM1 T1 T2, typeOfCI E2 E2' New1, flow New1 T1.
typeOfCI (tt) (tt) (bool).
typeOfCI (ff) (ff) (bool).
typeOfCI (emptyList T) (emptyList T) (list T).
typeOfCI (isnil T E) (isnil T (cast E' PM1 L (list T))) (bool) :- typeOfCI E E' PM1, matchList PM1 New1, flow New1 T.
typeOfCI (cons T E1 E2) (cons T (cast E1' New1 L T) (cast E2' PM1 L (list T))) (list T) :- typeOfCI E1 E1' New1, flow New1 T, typeOfCI E2 E2' PM1, matchList PM1 New2, flow New2 T.
typeOfCI (head T E) (head T (cast E' PM1 L (list T))) T :- typeOfCI E E' PM1, matchList PM1 New1, flow New1 T.
typeOfCI (tail T E) (tail T (cast E' PM1 L (list T))) T :- typeOfCI E E' PM1, matchList PM1 New1, flow New1 T.
value (cast V T L (dyn)) :- value V.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (list T1) L (list T1')) :- value V.
error (blame L).
step (cast V T L T) V :- value V.
step (cast E B L2 C) (cast V A L2 C) :- E = (cast V A L1 B), value V, flow A C.
step (cast E B L2 C) (blame L2) :- E = (cast V A L1 B), value V, not (flow A C).
step (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V.
step (isnil T (cast V (list T') L (list T))) (isnil T' V) :- value V.
step (head T (cast V (list T') L (list T))) (cast (head T' V) T' L T) :- value V.
step (tail T (cast V (list T') L (list T))) (cast (tail T' V) T' L T) :- value V.
matchInt (int).
matchInt (dyn).
matchArrow (arrow T1 T2) T1 T2.
matchArrow (dyn) (dyn) (dyn).
matchBool (bool).
matchBool (dyn).
matchList (list T1) T1.
matchList (dyn) (dyn).
matchDyn (dyn).
flow X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 X X X.
