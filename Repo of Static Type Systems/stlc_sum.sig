sig stlc_sum.

kind	term			type.
kind	typ				type.

type 	int			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type		plus		typ -> typ -> typ.

type		case 		term -> (term -> term) -> (term -> term) -> term.
type		inl			typ -> term -> term.
type		inr			typ -> term -> term.


% operatorInfo abs :- constructor, arrow.
% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% operatorInfo case :- deconstructor, plus, 1, covariant.
% operatorInfo inl :- constructor, plus.
% operatorInfo inr :- constructor, plus.
% context app 1[], 2[1].
% context case 1[].
% mode typeOf inp -> out.
% mode step inp -> out.
