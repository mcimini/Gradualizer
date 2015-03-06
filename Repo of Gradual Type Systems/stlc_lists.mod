module stlc_lists.

typeOf (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOf x T1 => typeOf (R x) T2)).
typeOf (app E1 E2) T2 :- typeOf E1 (arrow T1 T2), typeOf E2 T1.
typeOf (isnil T E) (bool) :- typeOf E (list T).
typeOf (cons T E1 E2) (list T) :- typeOf E1 T, typeOf E2 (list T).
typeOf (head T E) T :- typeOf E (list T).
typeOf (tail T E) T :- typeOf E (list T).

typeOfGr (abs O122 R) (arrow O122 O123) :- (pi x\ (typeOfGr x O122 => typeOfGr (R x) O123)).
typeOfGr (app E1 E2) O125 :- typeOfGr E1 PM124, matcharrow PM124 T1 O125, typeOfGr E2 O126, consistency O126 T1.
typeOfGr (isnil O128 E) (bool) :- typeOfGr E PM127, matchlist PM127 O129, join2 O129 O128 JoinT.
typeOfGr (cons O131 E1 E2) (list JoinT) :- typeOfGr E1 O132, typeOfGr E2 PM130, matchlist PM130 O133, join3 O132 O133 O131 JoinT.
typeOfGr (head O135 E) JoinT :- typeOfGr E PM134, matchlist PM134 O136, join2 O136 O135 JoinT.
typeOfGr (tail O138 E) JoinT :- typeOfGr E PM137, matchlist PM137 O139, join2 O139 O138 JoinT.

typeOfCC (abs T1 R) (arrow T1 T2) :- (pi x\ (typeOfCC x T1 => typeOfCC (R x) T2)).
typeOfCC (app E1 E2) T2 :- typeOfCC E1 (arrow T1 T2), typeOfCC E2 T1.
typeOfCC (isnil T E) (bool) :- typeOfCC E (list T).
typeOfCC (cons T E1 E2) (list T) :- typeOfCC E1 T, typeOfCC E2 (list T).
typeOfCC (head T E) T :- typeOfCC E (list T).
typeOfCC (tail T E) T :- typeOfCC E (list T).
typeOfCC (cast E T1 Label T2) T2 :- typeOfCC E T1.

typeOfCI (abs O140 R) (abs O140 R') (arrow O140 O141) :- (pi x\ (typeOfCI x x O140 => typeOfCI (R x) (R' x) O141)).
typeOfCI (app E1 E2) (app (cast E1' PM142 L144 (arrow T1 O145)) (cast E2' O143 L146 T1)) O145 :- typeOfCI E1 E1' PM142, matcharrow PM142 T1 O145, typeOfCI E2 E2' O143, consistency O143 T1.
typeOfCI (isnil O148 E) (isnil JoinT (cast E' PM147 L150 (list JoinT))) (bool) :- typeOfCI E E' PM147, matchlist PM147 O149, join2 O149 O148 JoinT.
typeOfCI (cons O152 E1 E2) (cons JoinT (cast E1' O154 L155 JoinT) (cast E2' PM151 L156 (list JoinT))) (list JoinT) :- typeOfCI E1 E1' O154, typeOfCI E2 E2' PM151, matchlist PM151 O153, join3 O154 O153 O152 JoinT.
typeOfCI (head O158 E) (head JoinT (cast E' PM157 L160 (list JoinT))) JoinT :- typeOfCI E E' PM157, matchlist PM157 O159, join2 O159 O158 JoinT.
typeOfCI (tail O162 E) (tail JoinT (cast E' PM161 L164 (list JoinT))) JoinT :- typeOfCI E E' PM161, matchlist PM161 O163, join2 O163 O162 JoinT.

matchlist (list X1) X1.
matchlist (dyn) (dyn).


join2 X dyn X.
join2 dyn X X.
join2 X X X.
join2 (list X1) (list Y1) (list Z1) :- join2 X1 Y1 Z1.

consistency X1 X2 :- join2 X1 X2 JoinX.
