	.text
	.globl	fmaxnm_d
	.p2align	2
	.type	fmaxnm_d,@function
fmaxnm_d:
	fmaxnm	d2, d0, d1
	fmov	x0, d2
	ret
.Lfunc_end0:
	.size	fmaxnm_d, .Lfunc_end0-fmaxnm_d
