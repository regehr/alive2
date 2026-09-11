	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fcvt_int_rounding
	.type	fcvt_int_rounding,@function
fcvt_int_rounding:
	# W to S, RUP: 16777217 rounds up to 16777218.
	li t1, 16777217
	fcvt.s.w ft0, t1, rup
	fmv.x.w t0, ft0
	sd	t0, 0(a0)

	# W to S, RDN: -16777217 rounds down to -16777218.
	li t1, -16777217
	fcvt.s.w ft0, t1, rdn
	fmv.x.w t0, ft0
	sd	t0, 8(a0)

	# WU to S, RTZ: UINT32_MAX rounds to 4294967040.
	li t1, -1
	fcvt.s.wu ft0, t1, rtz
	fmv.x.w t0, ft0
	sd	t0, 16(a0)

	# L to D, RMM: 2^53+1 is a tie rounded away from zero.
	li t1, 9007199254740993
	fcvt.d.l ft0, t1, rmm
	fmv.x.d t0, ft0
	sd	t0, 24(a0)

	# LU to D, RTZ: UINT64_MAX rounds toward zero.
	li t1, -1
	fcvt.d.lu ft0, t1, rtz
	fmv.x.d t0, ft0
	sd	t0, 32(a0)

	# W to H, RTZ: overflow clamps to the largest finite half.
	li t1, 65536
	fcvt.h.w ft0, t1, rtz
	fmv.x.h t0, ft0
	sd	t0, 40(a0)

	# L to S, RMM: negative tie rounds away from zero.
	li t1, -16777217
	fcvt.s.l ft0, t1, rmm
	fmv.x.w t0, ft0
	sd	t0, 48(a0)

	# LU to S, RUP: UINT64_MAX rounds up to 2^64.
	li t1, -1
	fcvt.s.lu ft0, t1, rup
	fmv.x.w t0, ft0
	sd	t0, 56(a0)

	ret
	.size	fcvt_int_rounding, .-fcvt_int_rounding
