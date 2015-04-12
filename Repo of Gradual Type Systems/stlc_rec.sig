sig stlc_rec.

accum_sig stlc_unit.

kind	field	type. 

type		rec3Type		field -> typ -> field -> typ -> field -> typ -> typ.


type		rec3			field -> term -> field -> term -> field -> term -> term.

type		proj1			term -> field -> term.
type		proj2			term -> field -> term.
type		proj3			term -> field -> term.

type		matchrec3Type		typ -> field -> typ -> field -> typ -> field -> typ -> o. 