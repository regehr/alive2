	.text
	.globl	fmaxnm_s
	.p2align	2
	.type	fmaxnm_s,@function
fmaxnm_s:
	fmaxnm	s2, s0, s1
	fmov	w0, s2
	ret
.Lfunc_end0:
	.size	fmaxnm_s, .Lfunc_end0-fmaxnm_s
