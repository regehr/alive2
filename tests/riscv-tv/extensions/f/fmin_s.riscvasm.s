	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2"
	.text
	.globl	fmin_s
	.type	fmin_s,@function
fmin_s:
	fmin.s	ft0, fa0, fa1
	fmv.x.w	a0, ft0
	ret
	.size	fmin_s, .-fmin_s
