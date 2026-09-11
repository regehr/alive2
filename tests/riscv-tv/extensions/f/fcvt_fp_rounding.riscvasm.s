	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fcvt_fp_rounding
	.type	fcvt_fp_rounding,@function
fcvt_fp_rounding:
	# D to S, RUP: 1+2^-24 rounds up to the next single.
	li t1, 0x3ff0000010000000
	fmv.d.x ft0, t1
	fcvt.s.d ft1, ft0, rup
	fmv.x.w t0, ft1
	sd	t0, 0(a0)

	# S to H, RMM: 1+2^-11 rounds away from zero.
	li t1, 0x3f801000
	fmv.w.x ft0, t1
	fcvt.h.s ft1, ft0, rmm
	fmv.x.h t0, ft1
	sd	t0, 8(a0)

	# H to D, RTZ: widening of 1 is exact.
	li t1, 0x3c00
	fmv.h.x ft0, t1
	fcvt.d.h ft1, ft0, rtz
	fmv.x.d t0, ft1
	sd	t0, 16(a0)

	ret
	.size	fcvt_fp_rounding, .-fcvt_fp_rounding
