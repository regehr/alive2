	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	nanbox_read
	.type	nanbox_read,@function
nanbox_read:
	# Unboxed single-precision zero is classified as a quiet NaN.
	fmv.d.x ft0, zero
	fclass.s t0, ft0
	sd	t0, 0(a0)

	# Unboxed half-precision zero is classified as a quiet NaN.
	fmv.d.x ft0, zero
	fclass.h t0, ft0
	sd	t0, 8(a0)

	# Arithmetic checks boxing before using narrow operands.
	fmv.d.x ft0, zero
	fadd.s ft1, ft0, ft0
	fmv.x.w t0, ft1
	sd	t0, 16(a0)

	# Sign injection checks boxing; negation then sets the NaN sign.
	fmv.d.x ft0, zero
	fsgnjn.s ft1, ft0, ft0
	fmv.x.w t0, ft1
	sd	t0, 24(a0)

	# Transfers out ignore boxing and preserve the low bits.
	li t1, 0x123456788000beef
	fmv.d.x ft0, t1
	fmv.x.w t0, ft0
	sd	t0, 32(a0)

	# Half-precision transfers out also ignore boxing.
	fmv.x.h t0, ft0
	sd	t0, 40(a0)

	# Stores ignore boxing as well.
	fsw ft0, 56(a0)
	lw t0, 56(a0)
	sd	t0, 48(a0)

	# Valid boxes preserve noncanonical NaN payloads on sign injection.
	li t1, 0xffffffff7fc00080
	fmv.d.x ft0, t1
	fsgnj.s ft1, ft0, ft0
	fmv.x.w t0, ft1
	sd	t0, 56(a0)

	ret
	.size	nanbox_read, .-nanbox_read
