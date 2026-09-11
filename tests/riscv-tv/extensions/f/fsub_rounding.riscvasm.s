	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fsub_rounding
	.type	fsub_rounding,@function
fsub_rounding:
	# -1-2^-24, RDN, is the next single below -1.
	li t1, 0x33800000
	fmv.w.x ft0, t1
	fli.s ft1, -1.0
	fsub.s ft2, ft1, ft0, rdn
	fmv.x.w t0, ft2
	sd	t0, 0(a0)

	ret
	.size	fsub_rounding, .-fsub_rounding
