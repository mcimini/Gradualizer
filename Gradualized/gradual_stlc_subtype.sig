sig gradual_stlc_subtype.


kind	term			type.
kind	typ				type.

type	int			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.
type	subtype			typ -> typ -> o.

% operatorInfo abs :- constructor, arrow.
% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% context app 1[], 2[1].
% mode typeOf inp -> out.
% mode step inp -> out.
% mode subtype inp -> inp.



kind label	 type.

type dyn		typ.
type typeOfGr		term -> typ -> o.
type typeOfCC		term -> typ -> o.
type cast		term -> typ -> label -> typ -> term.
type blame		label -> term.
type typeOfCI		term -> term -> typ -> o.
type matchInt		typ -> o.
type matchArrow		typ -> typ -> typ -> o.
type matchDyn		typ -> o.
type flow		typ -> typ -> o.
type join2		typ -> typ -> typ -> o.
type subtypeG		typ -> typ -> o.
type subtypeCI		typ -> typ -> typ -> o.
type ground		typ -> o.
type getGroundOf		typ -> typ -> o.
type sameGround		typ -> typ -> o.
type error		term -> o.
