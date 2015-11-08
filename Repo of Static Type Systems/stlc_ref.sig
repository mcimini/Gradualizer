sig stlc_ref.

kind	term			type.
kind	typ				type.

type	int			typ.
type	arrow		typ -> typ -> typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.

type	typeOf			term -> typ -> o. 

type	step			term -> heap -> term -> heap -> o.
type	value			term -> o.

type		refType		typ -> typ.
type		unitType	typ.

type		unit		term.
type		ref			term -> term.
type		deref		term -> term.
type		assign		term -> term -> term.

kind 	heap				type.
kind 	address			type.

type		loc			address -> term. 
type 		emptyHeap	heap.
type 		entry		address -> term -> heap -> heap.

type		lookup		address -> heap -> term -> o.
type		fresh		address -> heap -> o.
type		typeOfRuntype	address -> typ -> o. 


% operatorInfo abs :- constructor, arrow.
% operatorInfo app :- deconstructor, arrow, 1, contravariant, 1.
% operatorInfo ref :- constructor, refType.
% operatorInfo deref :- deconstructor, refType, 1, covariant.
% operatorInfo assign :- deconstructor, refType, 1, contravariant, 1.
% context app 1[], 2[1].
% context ref 1[].
% context deref 1[].
% context assign 1[],2[1].
% mode typeOf inp -> out.
% mode step inp -> inp -> out -> out.
% mode typeOfRuntype inp -> out.
% mode lookup inp -> inp -> out -> out.
% mode fresh inp -> inp.

