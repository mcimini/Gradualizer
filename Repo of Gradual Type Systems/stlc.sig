sig stlc.

kind	term			type.
kind	typ				type.
kind	label			type.

type	int			typ.
type	bool		typ.
type	arrow		typ -> typ -> typ.

type	dyn			typ.

type    app			term -> term -> term.
type    abs			typ -> (term -> term) -> term.
type    cast		term -> typ -> label -> typ -> term.

type	typeOf			term -> typ -> o. 
type	typeOfGr		term -> typ -> o. 
type	typeOfCC		term -> typ -> o. 
type	typeOfCI		term -> term -> typ -> o. 


type matcharrow		typ -> typ -> typ -> o.
type join2				typ -> typ -> typ -> o.
type consistency		typ -> typ -> o.

