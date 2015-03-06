module stlc_sum.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (case E R1 R2) T :- typeOf E (plus T1 T2), (pi x\ (typeOf x T1 => typeOf (R1 x) T)), (pi x\ (typeOf x T2 => typeOf (R2 x) T)).
typeOf (inl T2 E) (plus T1 T2) :- typeOf E T1.
typeOf (inr T1 E) (plus T1 T2) :- typeOf E T2.

typeOfGr (abs O13 R) (arrow O13 O14) :- (pi x\ (typeOfGr x O13 => typeOfGr (R x) O14)).
typeOfGr (app E1 E2) O16 :- typeOfGr E1 PM15, matcharrow PM15 T1 O16, typeOfGr E2 O17, consistency O17 T1.
typeOfGr (case E R1 R2) JoinT :- typeOfGr E PM18, matchplus PM18 O19 O20, (pi x\ (typeOfGr x O19 => typeOfGr (R1 x) O21)), (pi x\ (typeOfGr x O20 => typeOfGr (R2 x) O22)), join2 O21 O22 JoinT.
typeOfGr (inl O23 E) (plus O24 O23) :- typeOfGr E O24.
typeOfGr (inr O25 E) (plus O25 O26) :- typeOfGr E O26.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (case E R1 R2) T :- typeOfCC E (plus T1 T2), (pi x\ (typeOfCC x T1 => typeOfCC (R1 x) T)), (pi x\ (typeOfCC x T2 => typeOfCC (R2 x) T)).
typeOfCC (inl T2 E) (plus T1 T2) :- typeOfCC E T1.
typeOfCC (inr T1 E) (plus T1 T2) :- typeOfCC E T2.
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O27 R) (abs O27 R') (arrow O27 O28) :- (pi x\ (typeOfCI x x O27 => typeOfCI (R x) (R' x) O28)).
typeOfCI (app E1 E2) (app (cast E1' PM29 L31 (arrow T1 O32)) (cast E2' O30 L33 T1)) O32 :- typeOfCI E1 E1' PM29, matcharrow PM29 T1 O32, typeOfCI E2 E2' O30, consistency O30 T1.
typeOfCI (case E R1 R2) (case (cast E' PM34 L39 (plus O35 O36)) (x\ (cast (R1' x) O38 L40 JoinT)) (x\ (cast (R2' x) O37 L41 JoinT))) JoinT :- typeOfCI E E' PM34, matchplus PM34 O35 O36, (pi x\ (typeOfCI x x O35 => typeOfCI (R1 x) (R1' x) O38)), (pi x\ (typeOfCI x x O36 => typeOfCI (R2 x) (R2' x) O37)), join2 O38 O37 JoinT.
typeOfCI (inl O42 E) (inl O42 E') (plus O43 O42) :- typeOfCI E E' O43.
typeOfCI (inr O44 E) (inr O44 E') (plus O44 O45) :- typeOfCI E E' O45.

matchplus (plus X1 X2) X1 X2.
matchplus (dyn) (dyn) (dyn).


join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (plus X1 X2) (plus Y1 Y2) (plus Z1 Z2) :- join2 X1 Y1 Z1, join2 X2 Y2 Z2.

consistency X1 X2 :- join2 X1 X2 JoinX.
