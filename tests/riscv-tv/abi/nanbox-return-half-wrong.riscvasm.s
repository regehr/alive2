	.text
	.globl	nanbox_return_half_wrong
	.type	nanbox_return_half_wrong,@function
nanbox_return_half_wrong:
	li	t0, 0x7e00
	fmv.w.x	fa0, t0
	ret
.Lfunc_end0:
	.size	nanbox_return_half_wrong, .-nanbox_return_half_wrong
