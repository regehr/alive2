	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fmul_rounding
	.type	fmul_rounding,@function
fmul_rounding:
	# (1+2^-23)^2, RUP, is 1+3*2^-23.
	li t1, 0x3f800001
	fmv.w.x ft0, t1
	fmul.s ft1, ft0, ft0, rup
	fmv.x.w t0, ft1
	sd	t0, 0(a0)

	ret
	.size	fmul_rounding, .-fmul_rounding
