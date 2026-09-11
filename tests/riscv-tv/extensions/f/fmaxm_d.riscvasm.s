	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0"
	.text
	.globl	fmaxm_d
	.type	fmaxm_d,@function
fmaxm_d:
	fmaxm.d	ft0, fa0, fa1
	fmv.x.d	a0, ft0
	ret
	.size	fmaxm_d, .-fmaxm_d
