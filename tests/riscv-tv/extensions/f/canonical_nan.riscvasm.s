	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfh1p0"
	.text
	.globl	canonical_nan
	.type	canonical_nan,@function
canonical_nan:
	# FCVT.S.H canonicalizes a half-precision signaling NaN.
	lui	t0, 8
	addi	t0, t0, -1008       # 0x7c10
	fmv.h.x	ft0, t0
	fcvt.s.h	ft1, ft0
	fmv.x.w	a0, ft1          # 0x7fc00000

	# FSQRT.S canonicalizes a single-precision NaN with a payload.
	lui	t0, 0x7fc00
	addi	t0, t0, 128         # 0x7fc00080
	fmv.w.x	ft0, t0
	fsqrt.s	ft1, ft0
	fmv.x.w	a1, ft1          # 0x7fc00000
	xor	a0, a0, a1           # 0

	# FCVT.H.S canonicalizes the same noncanonical single-precision NaN.
	fcvt.h.s	ft1, ft0
	fmv.x.h	a1, ft1          # 0x7e00
	xor	a0, a0, a1           # 0x7e00

	# FSGNJ.S is bitwise and must preserve the NaN payload.
	fsgnj.s	ft1, ft0, ft0
	fmv.x.w	a1, ft1          # 0x7fc00080
	xor	a1, a1, t0           # 0
	xor	a0, a0, a1           # 0x7e00
	ret
	.size	canonical_nan, .-canonical_nan
