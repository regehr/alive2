	.text
	.globl	fminnm_wrong_snan
	.p2align	2
	.type	fminnm_wrong_snan,@function
fminnm_wrong_snan:
	// Deliberately incorrect: Quieting before FMINNM incorrectly suppresses a lone signaling NaN.
	fminnm	s0, s0, s0
	fminnm	s1, s1, s1
	fminnm	s2, s0, s1
	fmov	w0, s2
	ret
.Lfunc_end0:
	.size	fminnm_wrong_snan, .Lfunc_end0-fminnm_wrong_snan
