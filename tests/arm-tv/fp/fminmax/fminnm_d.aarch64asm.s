	.text
	.globl	fminnm_d
	.p2align	2
	.type	fminnm_d,@function
fminnm_d:
	fminnm	d2, d0, d1
	fmov	x0, d2
	ret
.Lfunc_end0:
	.size	fminnm_d, .Lfunc_end0-fminnm_d
