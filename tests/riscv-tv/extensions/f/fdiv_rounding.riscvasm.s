	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fdiv_rounding
	.type	fdiv_rounding,@function
fdiv_rounding:
	# 1/3, RTZ, is the lower adjacent single.
	fli.s ft0, 1.0
	fli.s ft1, 3.0
	fdiv.s ft2, ft0, ft1, rtz
	fmv.x.w t0, ft2
	sd	t0, 0(a0)

	ret
	.size	fdiv_rounding, .-fdiv_rounding
