	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfh1p0"
	.text
	.globl	fmin_h
	.type	fmin_h,@function
fmin_h:
	fmin.h	ft0, fa0, fa1
	fmv.x.h	a0, ft0
	ret
	.size	fmin_h, .-fmin_h
