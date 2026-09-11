	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0"
	.text
	.globl	fcvtmod_wrap
	.type	fcvtmod_wrap,@function
fcvtmod_wrap:
	fcvt.d.w	ft0, a0
	li	t0, 0x41f0000000000000 # 2^32
	fmv.d.x	ft1, t0
	fli.d	ft2, 0.5
	fadd.d	ft3, ft0, ft1
	fadd.d	ft3, ft3, ft2     # positive x + 2^32 + 0.5
	fcvtmod.w.d	t0, ft3, rtz
	sd	t0, 0(a1)
	fsub.d	ft3, ft0, ft1
	fsub.d	ft3, ft3, ft2     # negative x - 2^32 - 0.5
	fcvtmod.w.d	t0, ft3, rtz
	sd	t0, 8(a1)
	ret
	.size	fcvtmod_wrap, .-fcvtmod_wrap
