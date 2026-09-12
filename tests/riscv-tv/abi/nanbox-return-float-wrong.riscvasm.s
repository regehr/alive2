	.text
	.globl	nanbox_return_float_wrong
	.type	nanbox_return_float_wrong,@function
nanbox_return_float_wrong:
	li	t0, 0x7fffffff3f800000
	fmv.d.x	fa0, t0
	ret
.Lfunc_end0:
	.size	nanbox_return_float_wrong, .-nanbox_return_float_wrong
