sig stlc_lists.

kind	term			type.
kind	typ				type.

type	int			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type	bool			typ.
type	list			typ -> typ.

type	tt		term.
type	ff		term.
type	emptyList	typ -> term.
type	isnil		typ -> term -> term. 
type	cons		typ -> term -> term -> term. 
type	head 		typ -> term -> term. 
type	tail 		typ -> term -> term. 

% operatorInfo abs :- constructor, arrow.
% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% operatorInfo emptyList :- constructor, list.
% operatorInfo cons :- constructor, list.
% operatorInfo isnil :- deconstructor, list, 1, covariant.
% operatorInfo head :- deconstructor, list, 1, covariant.
% operatorInfo tail :- deconstructor, list, 1, covariant.
% context app 1[], 2[1].
% context isnil 2[].
% context head 2[].
% context tail 2[].
% mode typeOf inp -> out.
% mode step inp -> out.

