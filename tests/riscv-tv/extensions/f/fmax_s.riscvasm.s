	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2"
	.text
	.globl	fmax_s
	.type	fmax_s,@function
fmax_s:
	fmax.s	ft0, fa0, fa1
	fmv.x.w	a0, ft0
	ret
	.size	fmax_s, .-fmax_s
