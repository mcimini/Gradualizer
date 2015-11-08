sig stlc_pairs.

kind	term			type.
kind	typ				type.

type	int			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type	pairType		typ -> typ -> typ.
type	pair			term -> term -> term.
type	fst				term -> term.
type	snd				term -> term.

% operatorInfo abs :- constructor, arrow.
% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% operatorInfo pair :- constructor, pairType.
% operatorInfo fst :- deconstructor, arrow, 1, covariant.
% operatorInfo snd :- deconstructor, arrow, 1, covariant.
% context app 1[], 2[1].
% context pair 1[],2[1].
% context fst 1[].
% context snd 1[].
% mode typeOf inp -> out.
% mode step inp -> out.

