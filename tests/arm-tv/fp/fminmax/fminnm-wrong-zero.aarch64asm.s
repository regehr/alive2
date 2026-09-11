	.text
	.globl	fminnm_wrong_zero
	.p2align	2
	.type	fminnm_wrong_zero,@function
fminnm_wrong_zero:
	// Deliberately incorrect: Choosing the first equal operand gets (+0,-0) wrong.
	fminnm	s2, s0, s1
	fcmp	s0, s1
	b.ne	.Ldone
	fmov	s2, s0
.Ldone:
	fmov	w0, s2
	ret
.Lfunc_end0:
	.size	fminnm_wrong_zero, .Lfunc_end0-fminnm_wrong_zero
