	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fnmadd_zero
	.type	fnmadd_zero,@function
fnmadd_zero:
	fli.s	ft0, 1.0
	fli.s	ft1, -1.0
	fnmadd.s	ft2, ft0, ft0, ft1, rne
	fmv.x.w	a0, ft2
	ret
	.size	fnmadd_zero, .-fnmadd_zero
