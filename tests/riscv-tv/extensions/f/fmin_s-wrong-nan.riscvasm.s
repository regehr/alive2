	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0"
	.text
	.globl	fmin_s_wrong_nan
	.type	fmin_s_wrong_nan,@function
fmin_s_wrong_nan:
	# Deliberately incorrect: Reject NaN propagation: a NaN and 1.0 must return 1.0, not a NaN.
	fminm.s	ft0, fa0, fa1
	fmv.x.w	a0, ft0
	ret
	.size	fmin_s_wrong_nan, .-fmin_s_wrong_nan
