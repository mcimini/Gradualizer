sig stlc_if.

kind	term			type.
kind	typ				type.

type	int			typ.
type	bool			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type		if		term -> term -> term -> term.
type		tt	term.
type		ff	term.

% operatorInfo abs :- constructor, arrow.
% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% operatorInfo if :- deconstructor, bool, 1, covariant, 1.
% context app 1[], 2[1].
% context if 1[], 2[1], 3[1,2].
% mode typeOf inp -> out.
% mode step inp -> out.

