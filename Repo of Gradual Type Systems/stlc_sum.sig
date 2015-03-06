sig stlc_sum.

accum_sig stlc.

type		plus		typ -> typ -> typ.

type		case 		term -> (term -> term) -> (term -> term) -> term.
type		inl			typ -> term -> term.
type		inr			typ -> term -> term.

type 		matchplus		typ -> typ -> typ -> o.