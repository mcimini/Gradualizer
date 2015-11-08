sig stlc_exc.

kind	term			type.
kind	typ				type.

type	int			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.
type	error			term -> o.

type		excType			typ.
type		raise			typ -> term -> term.
type		try				term -> term -> term.

% operatorInfo abs :- constructor, arrow.
% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% context app 1[], 2[1].
% context try 1[].
% context raise 1[].
% mode typeOf inp -> out.
% mode step inp -> out.

