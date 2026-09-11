	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfh1p0"
	.text
	.globl	fmax_h
	.type	fmax_h,@function
fmax_h:
	fmax.h	ft0, fa0, fa1
	fmv.x.h	a0, ft0
	ret
	.size	fmax_h, .-fmax_h
