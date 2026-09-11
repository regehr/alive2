	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fcvt_nan
	.type	fcvt_nan,@function
fcvt_nan:
	# FCVT.W.H maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0xfe01
	fmv.h.x ft0, t1
	fcvt.w.h t0, ft0, rtz
	sd	t0, 0(a0)

	# FCVT.WU.H maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0xfe01
	fmv.h.x ft0, t1
	fcvt.wu.h t0, ft0, rtz
	sd	t0, 8(a0)

	# FCVT.L.H maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0xfe01
	fmv.h.x ft0, t1
	fcvt.l.h t0, ft0, rtz
	sd	t0, 16(a0)

	# FCVT.LU.H maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0xfe01
	fmv.h.x ft0, t1
	fcvt.lu.h t0, ft0, rtz
	sd	t0, 24(a0)

	# FCVT.W.S maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0x7f800001
	fmv.w.x ft0, t1
	fcvt.w.s t0, ft0, rtz
	sd	t0, 32(a0)

	# FCVT.WU.S maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0x7f800001
	fmv.w.x ft0, t1
	fcvt.wu.s t0, ft0, rtz
	sd	t0, 40(a0)

	# FCVT.L.S maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0x7f800001
	fmv.w.x ft0, t1
	fcvt.l.s t0, ft0, rtz
	sd	t0, 48(a0)

	# FCVT.LU.S maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0x7f800001
	fmv.w.x ft0, t1
	fcvt.lu.s t0, ft0, rtz
	sd	t0, 56(a0)

	# FCVT.W.D maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0xfff8000000000001
	fmv.d.x ft0, t1
	fcvt.w.d t0, ft0, rtz
	sd	t0, 64(a0)

	# FCVT.WU.D maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0xfff8000000000001
	fmv.d.x ft0, t1
	fcvt.wu.d t0, ft0, rtz
	sd	t0, 72(a0)

	# FCVT.L.D maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0xfff8000000000001
	fmv.d.x ft0, t1
	fcvt.l.d t0, ft0, rtz
	sd	t0, 80(a0)

	# FCVT.LU.D maps NaN to the maximum integer; WU is sign-extended.
	li t1, 0xfff8000000000001
	fmv.d.x ft0, t1
	fcvt.lu.d t0, ft0, rtz
	sd	t0, 88(a0)

	ret
	.size	fcvt_nan, .-fcvt_nan
