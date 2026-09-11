	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fsqrt_rounding
	.type	fsqrt_rounding,@function
fsqrt_rounding:
	# sqrt(2), RUP, is the upper adjacent single.
	fli.s ft0, 2.0
	fsqrt.s ft1, ft0, rup
	fmv.x.w t0, ft1
	sd	t0, 0(a0)

	ret
	.size	fsqrt_rounding, .-fsqrt_rounding
