	.text
	.globl	fmin_d
	.p2align	2
	.type	fmin_d,@function
fmin_d:
	fmin	d2, d0, d1
	fmov	x0, d2
	ret
.Lfunc_end0:
	.size	fmin_d, .Lfunc_end0-fmin_d
