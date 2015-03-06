sig stlc_exc.

accum_sig stlc.

type		excType			typ.
type		excTypeFun		typ -> typ.

type		raise			typ -> term -> term.
type		try				term -> term -> term.

type		matchexcType			typ -> o.
type		matchexcTypeFun		typ -> typ -> o.