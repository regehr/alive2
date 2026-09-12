	.text
	.file	1 "foo.ll"
	.globl	nanbox_tail_half_wrong
	.type	nanbox_tail_half_wrong,@function
nanbox_tail_half_wrong:
	# fa0 is valid; fa1 is only boxed to single precision.
	fmv.w.x	fa1, a0
	.loc	1 1 0
	tail	consume
.Lfunc_end0:
	.size	nanbox_tail_half_wrong, .-nanbox_tail_half_wrong
