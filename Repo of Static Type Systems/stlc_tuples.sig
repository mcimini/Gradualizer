sig stlc_tuples.

kind	term			type.
kind	typ				type.

type	int			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> term -> o.
type	value			term -> o.

type	tupleType		typ -> typ -> typ -> typ -> typ.
type	tuple			term -> term -> term -> term -> term.
type	select1				term -> term.
type	select2				term -> term.
type	select3				term -> term.
type	select4				term -> term.

% operatorInfo abs :- constructor, arrow.
% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% operatorInfo tuple :- constructor, tupleType.
% operatorInfo select1 :- deconstructor, tupleType, 1, covariant.
% operatorInfo select2 :- deconstructor, tupleType, 1, covariant.
% operatorInfo select3 :- deconstructor, tupleType, 1, covariant.
% operatorInfo select4 :- deconstructor, tupleType, 1, covariant.
% context app 1[], 2[1].
% context tuple 1[],2[1],3[1,2],4[1,2,3].
% context select2 1[].
% context select3 1[].
% context select4 1[].
% mode typeOf inp -> out.
% mode step inp -> out.
