	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0"
	.text
	.globl	fcvtmod_w_d
	.type	fcvtmod_w_d,@function
fcvtmod_w_d:
	# +0 -> 0
	li	t0, 0x0000000000000000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 0(a0)

	# -0 -> 0
	li	t0, 0x8000000000000000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 8(a0)

	# minimum positive subnormal -> 0
	li	t0, 0x0000000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 16(a0)

	# minimum negative subnormal -> 0
	li	t0, 0x8000000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 24(a0)

	# 0.75 -> 0
	li	t0, 0x3fe8000000000000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 32(a0)

	# -0.75 -> 0
	li	t0, 0xbfe8000000000000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 40(a0)

	# 1.75 -> 1
	li	t0, 0x3ffc000000000000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 48(a0)

	# -1.75 -> -1
	li	t0, 0xbffc000000000000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 56(a0)

	# 2^31-0.5 -> 2147483647
	li	t0, 0x41dfffffffe00000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 64(a0)

	# 2^31+0.5 -> -2147483648
	li	t0, 0x41e0000000100000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 72(a0)

	# -2^31-0.5 -> -2147483648
	li	t0, 0xc1e0000000100000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 80(a0)

	# 2^32-0.5 -> -1
	li	t0, 0x41effffffff00000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 88(a0)

	# -2^32+0.5 -> 1
	li	t0, 0xc1effffffff00000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 96(a0)

	# 2^32+1.75 -> 1
	li	t0, 0x41f00000001c0000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 104(a0)

	# -2^32-1.75 -> -1
	li	t0, 0xc1f00000001c0000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 112(a0)

	# 2^51+1.5 -> 1
	li	t0, 0x4320000000000003
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 120(a0)

	# 2^52+1 -> 1
	li	t0, 0x4330000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 128(a0)

	# 2^53-1 -> -1
	li	t0, 0x433fffffffffffff
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 136(a0)

	# 2^53+2 -> 2
	li	t0, 0x4340000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 144(a0)

	# 2^63+2048 -> 2048
	li	t0, 0x43e0000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 152(a0)

	# 2^64+4096 -> 4096
	li	t0, 0x43f0000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 160(a0)

	# -2^64-4096 -> -4096
	li	t0, 0xc3f0000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 168(a0)

	# 2^83+2^31 -> -2147483648
	li	t0, 0x4520000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 176(a0)

	# -2^83-2^31 -> -2147483648
	li	t0, 0xc520000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 184(a0)

	# largest double below 2^84 -> -2147483648
	li	t0, 0x452fffffffffffff
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 192(a0)

	# 2^84+2^32 -> 0
	li	t0, 0x4530000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 200(a0)

	# maximum finite double -> 0
	li	t0, 0x7fefffffffffffff
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 208(a0)

	# minimum finite double -> 0
	li	t0, 0xffefffffffffffff
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 216(a0)

	# +infinity -> 0
	li	t0, 0x7ff0000000000000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 224(a0)

	# -infinity -> 0
	li	t0, 0xfff0000000000000
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 232(a0)

	# quiet NaN with payload -> 0
	li	t0, 0x7ff8000000000123
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 240(a0)

	# negative quiet NaN -> 0
	li	t0, 0xfff8000000000123
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 248(a0)

	# signaling NaN -> 0
	li	t0, 0x7ff0000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 256(a0)

	# negative signaling NaN -> 0
	li	t0, 0xfff0000000000001
	fmv.d.x	ft0, t0
	fcvtmod.w.d	t0, ft0, rtz
	sd	t0, 264(a0)

	ret
	.size	fcvtmod_w_d, .-fcvtmod_w_d
