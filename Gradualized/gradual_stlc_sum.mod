module gradual_stlc_sum.


typeOf (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (E x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (case E E1 E2) T :- typeOf E (plus T1 T2), (pi x\ (typeOf x T1 => typeOf (E1 x) T)), (pi x\ (typeOf x T2 => typeOf (E2 x) T)).
typeOf (inl T2 E) (plus T1 T2) :- typeOf E T1.
typeOf (inr T1 E) (plus T1 T2) :- typeOf E T2.

value (abs T E).
value (inl T V) :- value V.
value (inr T V) :- value V.

step (case (inl T V) E1 E2) (E1 V) :- value V.
step (case (inr T V) E1 E2) (E2 V) :- value V.


typeOfGr (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfGr x T1 => typeOfGr (E x) T2)).
typeOfGr (app E1 E2) T2 :- typeOfGr E1 PM1, matchArrow PM1 T1 T2, typeOfGr E2 New1, flow New1 T1.
typeOfGr (case E E1 E2) JoinT :- typeOfGr E PM1, matchPlus PM1 T1 T2, (pi x\ (typeOfGr x T1 => typeOfGr (E1 x) New1)), flow New1 JoinT, (pi x\ (typeOfGr x T2 => typeOfGr (E2 x) New2)), flow New2 JoinT, join2 New2 New1 JoinT.
typeOfGr (inl T2 E) (plus T1 T2) :- typeOfGr E T1.
typeOfGr (inr T1 E) (plus T1 T2) :- typeOfGr E T2.
typeOfCC (abs T1 E) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (E x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (case E E1 E2) T :- typeOfCC E (plus T1 T2), (pi x\ (typeOfCC x T1 => typeOfCC (E1 x) T)), (pi x\ (typeOfCC x T2 => typeOfCC (E2 x) T)).
typeOfCC (inl T2 E) (plus T1 T2) :- typeOfCC E T1.
typeOfCC (inr T1 E) (plus T1 T2) :- typeOfCC E T2.
typeOfCC (cast E T1 L T2) T2 :- typeOfCC E T1.
typeOfCC (blame L) T.
typeOfCI (abs T1 E) (abs T1 (x\ (E' x))) (arrow T1 T2) :- (pi x\ (typeOfCI x x T1 => typeOfCI (E x) (E' x) T2)).
typeOfCI (app E1 E2) (app (cast E1' PM1 L (arrow T1 T2)) (cast E2' New1 L T1)) T2 :- typeOfCI E1 E1' PM1, matchArrow PM1 T1 T2, typeOfCI E2 E2' New1, flow New1 T1.
typeOfCI (case E E1 E2) (case (cast E' PM1 L (plus T1 T2)) (x\ (cast (E1' x) New1 L JoinT)) (x\ (cast (E2' x) New2 L JoinT))) JoinT :- typeOfCI E E' PM1, matchPlus PM1 T1 T2, (pi x\ (typeOfCI x x T1 => typeOfCI (E1 x) (E1' x) New1)), flow New1 JoinT, (pi x\ (typeOfCI x x T2 => typeOfCI (E2 x) (E2' x) New2)), flow New2 JoinT, join2 New2 New1 JoinT.
typeOfCI (inl T2 E) (inl T2 E') (plus T1 T2) :- typeOfCI E E' T1.
typeOfCI (inr T1 E) (inr T1 E') (plus T1 T2) :- typeOfCI E E' T2.
value (cast V T L (dyn)) :- value V.
value (cast V (arrow T1 T2) L (arrow T1' T2')) :- value V.
value (cast V (plus T1 T2) L (plus T1' T2')) :- value V.
error (blame L).
step (cast V T L T) V :- value V.
step (cast E B L2 C) (cast V A L2 C) :- E = (cast V A L1 B), value V, flow A C.
step (cast E B L2 C) (blame L2) :- E = (cast V A L1 B), value V, not (flow A C).
step (app (cast V (arrow T1' T2') L (arrow T1 T2)) E2) (cast (app V (cast E2 T1 L T1')) T2' L T2) :- value V.
step (case (cast V (plus T1' T2') L (plus T1 T2)) E1 E2) (case V (x\ (E1 (cast x T1 L T1'))) (x\ (E2 (cast x T2 L T2')))) :- value V.
matchInt (int).
matchInt (dyn).
matchArrow (arrow T1 T2) T1 T2.
matchArrow (dyn) (dyn) (dyn).
matchPlus (plus T1 T2) T1 T2.
matchPlus (dyn) (dyn) (dyn).
matchDyn (dyn).
flow X1 X2 :- join2 X1 X2 JoinX.
join2 (dyn) X X.
join2 X (dyn) X.
join2 X X X.
join2 (int) (int) (int).
join2 (arrow X1 X2) (arrow Y1 Y2) (arrow Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (plus X1 X2) (plus Y1 Y2) (plus Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.
join2 (dyn) (dyn) (dyn).
