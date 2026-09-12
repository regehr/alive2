	.text
	.globl	nanbox_return_half
	.type	nanbox_return_half,@function
nanbox_return_half:
	li	t0, -65536
	or	t0, a0, t0
	fmv.d.x	fa0, t0
	ret
.Lfunc_end0:
	.size	nanbox_return_half, .-nanbox_return_half
