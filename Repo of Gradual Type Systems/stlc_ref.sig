sig stlc_ref.

accum_sig stlc_unit.

type		refType		typ -> typ.

type		ref			term -> term.
type		deref		term -> term.
type		assign		term -> term -> term.

type		matchrefType		typ -> typ -> o. 