	.text
	.globl	nanbox_return_float
	.type	nanbox_return_float,@function
nanbox_return_float:
	fmv.w.x	fa0, a0
	ret
.Lfunc_end0:
	.size	nanbox_return_float, .-nanbox_return_float
