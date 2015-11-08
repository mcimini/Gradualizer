sig stlc_letrec.

kind	term			type.
kind	typ				type.

type	int			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type		fix		term -> term.
type		let		term -> (term -> term) -> term.
type		letrec		typ -> (term -> term) -> (term -> term) -> term.

% operatorInfo abs :- constructor, arrow.
% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% operatorInfo fix :- derived, arrow, 1, contravariant, 1.
% operatorInfo let :- derived.
% operatorInfo letrec :- derived.
% context app 1[], 2[1].
% context fix 1[].
% context let 1[].
% context letrec 1[].
% mode typeOf inp -> out.
% mode step inp -> out.
