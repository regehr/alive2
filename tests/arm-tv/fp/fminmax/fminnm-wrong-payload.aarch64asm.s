	.text
	.globl	fminnm_wrong_payload
	.p2align	2
	.type	fminnm_wrong_payload,@function
fminnm_wrong_payload:
	// Deliberately incorrect: Swapping operands changes which NaN payload wins.
	fminnm	s2, s1, s0
	fmov	w0, s2
	ret
.Lfunc_end0:
	.size	fminnm_wrong_payload, .Lfunc_end0-fminnm_wrong_payload
