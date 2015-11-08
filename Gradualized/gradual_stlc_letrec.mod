module gradual_stlc_letrec.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (fix E) T :- typeOf E (arrow T T).
typeOf (let E1 E2) T2 :- typeOf E1 T1, (pi x\ (typeOf x T1 => typeOf (E2 x) T2)).
typeOf (letrec T1 E1 E2) T2 :- (pi x\ (typeOf x T1 => typeOf (E1 x) T1)), (pi x\ (typeOf x T1 => typeOf (E2 x) T2)).

value (abs T E).

step (fix V) (app V (fix V)) :- value V.
step (let V E) (E V) :- value V.
step (letrec T E1 E2) (let (fix (abs T E1)) E2).


typeOfGr (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfGr x T1 => typeOfGr (E x) T2)).
typeOfGr (app E1 E2) T2 :- typeOfGr E1 PM1, matchArrow PM1 T1 T2, typeOfGr E2 New1, flow New1 T1.
typeOfGr (fix E) T :- typeOfGr E PM1, matchArrow PM1 T New1, flow New1 T.
typeOfGr (let E1 E2) T2 :- typeOfGr E1 T1, (pi x\ (typeOfGr x T1 => typeOfGr (E2 x) T2)).
typeOfGr (letrec T1 E1 E2) T2 :- (pi x\ (typeOfGr x T1 => typeOfGr (E1 x) New1)), flow New1 T1, (pi x\ (typeOfGr x T1 => typeOfGr (E2 x) T2)).
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (fix E) T :- typeOfCC E (arrow T T).
typeOfCC (let E1 E2) T2 :- typeOfCC E1 T1, (pi x\ (typeOfCC x T1 => typeOfCC (E2 x) T2)).
typeOfCC (letrec T1 E1 E2) T2 :- (pi x\ (typeOfCC x T1 => typeOfCC (E1 x) T1)), (pi x\ (typeOfCC x T1 => typeOfCC (E2 x) T2)).
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1.
typeOfCC (blame L) T.
typeOfCI (abs T1 E) (abs T1 (x\ (E' x))) (arrow T1 T2) :- (pi x\ (typeOfCI x x T1 => typeOfCI (E x) (E' x) T2)).
typeOfCI (app E1 E2) (app (cast E1' PM1 L (arrow T1 T2)) (cast E2' New1 L T1)) T2 :- typeOfCI E1 E1' PM1, matchArrow PM1 T1 T2, typeOfCI E2 E2' New1, flow New1 T1.
typeOfCI (fix E) (fix (cast E' PM1 L (arrow T T))) T :- typeOfCI E E' PM1, matchArrow PM1 T New1, flow New1 T.
typeOfCI (let E1 E2) (let E1' (x\ (E2' x))) T2 :- typeOfCI E1 E1' T1, (pi x\ (typeOfCI x x T1 => typeOfCI (E2 x) (E2' x) T2)).
typeOfCI (letrec T1 E1 E2) (letrec T1 (x\ (cast (E1' x) New1 L T1)) (x\ (E2' x))) T2 :- (pi x\ (typeOfCI x x T1 => typeOfCI (E1 x) (E1' x) New1)), flow New1 T1, (pi x\ (typeOfCI x x T1 => typeOfCI (E2 x) (E2' x) T2)).
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
