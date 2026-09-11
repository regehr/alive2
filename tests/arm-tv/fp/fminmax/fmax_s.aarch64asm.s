	.text
	.globl	fmax_s
	.p2align	2
	.type	fmax_s,@function
fmax_s:
	fmax	s2, s0, s1
	fmov	w0, s2
	ret
.Lfunc_end0:
	.size	fmax_s, .Lfunc_end0-fmax_s
