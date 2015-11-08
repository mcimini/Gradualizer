sig stlc_inference.

kind	term			type.
kind	typ				type.

type	int			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type		absInf		(term -> term) -> term.

% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% operatorInfo absInf :- constructor, arrow.
% context app 1[], 2[1].
% mode typeOf inp -> out.
% mode step inp -> out.

