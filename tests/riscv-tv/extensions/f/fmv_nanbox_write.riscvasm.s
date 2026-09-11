	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	fmv_nanbox_write
	.type	fmv_nanbox_write,@function
fmv_nanbox_write:
	fmv.w.x	ft0, a0
	fmv.x.d	t0, ft0
	sd	t0, 0(a1)
	fmv.h.x	ft0, a0
	fmv.x.d	t0, ft0
	sd	t0, 8(a1)
	fmv.d.x	ft0, a0
	fmv.x.d	t0, ft0
	sd	t0, 16(a1)
	ret
	.size	fmv_nanbox_write, .-fmv_nanbox_write
