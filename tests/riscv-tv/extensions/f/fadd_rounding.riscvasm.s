	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fadd_rounding
	.type	fadd_rounding,@function
fadd_rounding:
	# 1+2^-24, RUP, is the next single above 1.
	li t1, 0x33800000
	fmv.w.x ft0, t1
	fli.s ft1, 1.0
	fadd.s ft2, ft1, ft0, rup
	fmv.x.w t0, ft2
	sd	t0, 0(a0)

	# -1-2^-24, RDN, is the next single below -1.
	fsgnjn.s ft0, ft0, ft0
	fsgnjn.s ft1, ft1, ft1
	fadd.s ft2, ft1, ft0, rdn
	fmv.x.w t0, ft2
	sd	t0, 8(a0)

	ret
	.size	fadd_rounding, .-fadd_rounding
