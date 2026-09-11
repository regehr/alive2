	.text
	.globl	fminnm_s
	.p2align	2
	.type	fminnm_s,@function
fminnm_s:
	fminnm	s2, s0, s1
	fmov	w0, s2
	ret
.Lfunc_end0:
	.size	fminnm_s, .Lfunc_end0-fminnm_s
