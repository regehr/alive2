	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2"
	.text
	.globl	fmin_s_wrong_zero
	.type	fmin_s_wrong_zero,@function
fmin_s_wrong_zero:
	# Deliberately incorrect: Reject choosing the first equal operand: (+0.0, -0.0) must return -0.0.
	fmin.s	ft0, fa0, fa1
	feq.s	t0, fa0, fa1
	beqz	t0, .Ldone
	fsgnj.s	ft0, fa0, fa0
.Ldone:
	fmv.x.w	a0, ft0
	ret
	.size	fmin_s_wrong_zero, .-fmin_s_wrong_zero
