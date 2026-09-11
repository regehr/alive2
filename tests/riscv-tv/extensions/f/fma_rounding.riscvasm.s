	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fma_rounding
	.type	fma_rounding,@function
fma_rounding:
	# FMADD: 1*1+2^-24, RUP.
	fli.s ft0, 1.0
	li t1, 0x33800000
	fmv.w.x ft1, t1
	fmadd.s ft2, ft0, ft0, ft1, rup
	fmv.x.w t0, ft2
	sd	t0, 0(a0)

	# FMSUB: 1*1-(-2^-24), RUP.
	fsgnjn.s ft1, ft1, ft1
	fmsub.s ft2, ft0, ft0, ft1, rup
	fmv.x.w t0, ft2
	sd	t0, 8(a0)

	# FNMADD: -(1*1)-2^-24, RDN.
	fsgnjn.s ft1, ft1, ft1
	fnmadd.s ft2, ft0, ft0, ft1, rdn
	fmv.x.w t0, ft2
	sd	t0, 16(a0)

	# FNMSUB: -(1*1)+(-2^-24), RDN.
	fsgnjn.s ft1, ft1, ft1
	fnmsub.s ft2, ft0, ft0, ft1, rdn
	fmv.x.w t0, ft2
	sd	t0, 24(a0)

	ret
	.size	fma_rounding, .-fma_rounding
