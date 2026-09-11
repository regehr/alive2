	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2"
	.text
	.globl	fmin_s_wrong_order
	.type	fmin_s_wrong_order,@function
fmin_s_wrong_order:
	# Deliberately incorrect: Reject FMAX in place of FMIN: inputs 1.0 and 2.0 must return 1.0.
	fmax.s	ft0, fa0, fa1
	fmv.x.w	a0, ft0
	ret
	.size	fmin_s_wrong_order, .-fmin_s_wrong_order
