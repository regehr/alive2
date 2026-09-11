	.text
	.globl	fmin_s
	.p2align	2
	.type	fmin_s,@function
fmin_s:
	fmin	s2, s0, s1
	fmov	w0, s2
	ret
.Lfunc_end0:
	.size	fmin_s, .Lfunc_end0-fmin_s
