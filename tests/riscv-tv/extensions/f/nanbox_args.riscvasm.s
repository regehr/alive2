	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_zfa1p0_zfh1p0"
	.text
	.globl	nanbox_args
	.type	nanbox_args,@function
nanbox_args:
	fmv.x.d	t0, fa0
	sd	t0, 0(a0)
	fmv.x.d	t0, fa1
	sd	t0, 8(a0)
	ret
	.size	nanbox_args, .-nanbox_args
