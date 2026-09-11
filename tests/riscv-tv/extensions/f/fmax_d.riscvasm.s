	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2"
	.text
	.globl	fmax_d
	.type	fmax_d,@function
fmax_d:
	fmax.d	ft0, fa0, fa1
	fmv.x.d	a0, ft0
	ret
	.size	fmax_d, .-fmax_d
