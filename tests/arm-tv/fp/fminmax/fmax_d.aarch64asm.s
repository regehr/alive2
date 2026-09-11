	.text
	.globl	fmax_d
	.p2align	2
	.type	fmax_d,@function
fmax_d:
	fmax	d2, d0, d1
	fmov	x0, d2
	ret
.Lfunc_end0:
	.size	fmax_d, .Lfunc_end0-fmax_d
