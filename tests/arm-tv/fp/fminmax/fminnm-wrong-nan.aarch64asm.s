	.text
	.globl	fminnm_wrong_nan
	.p2align	2
	.type	fminnm_wrong_nan,@function
fminnm_wrong_nan:
	// Deliberately incorrect: FMIN propagates a lone quiet NaN that FMINNM must suppress.
	fmin	s2, s0, s1
	fmov	w0, s2
	ret
.Lfunc_end0:
	.size	fminnm_wrong_nan, .Lfunc_end0-fminnm_wrong_nan
