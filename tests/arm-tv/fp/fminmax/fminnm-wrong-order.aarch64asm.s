	.text
	.globl	fminnm_wrong_order
	.p2align	2
	.type	fminnm_wrong_order,@function
fminnm_wrong_order:
	// Deliberately incorrect: FMAXNM instead of FMINNM reverses numerical ordering.
	fmaxnm	s2, s0, s1
	fmov	w0, s2
	ret
.Lfunc_end0:
	.size	fminnm_wrong_order, .Lfunc_end0-fminnm_wrong_order
