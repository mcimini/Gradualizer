sig stlc_lists.

accum_sig stlc.

type	list		typ -> typ.

type	emptyList	typ -> term.
type	isnil		typ -> term -> term. 
type	cons		typ -> term -> term -> term. 
type	head 		typ -> term -> term. 
type	tail 		typ -> term -> term. 

type	matchlist		typ -> typ -> o.
type 	join3			typ -> typ -> typ -> typ -> o.
