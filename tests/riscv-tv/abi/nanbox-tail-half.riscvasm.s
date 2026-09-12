	.text
	.file	1 "foo.ll"
	.globl	nanbox_tail_half
	.type	nanbox_tail_half,@function
nanbox_tail_half:
	li	t0, -65536
	or	t0, a0, t0
	fmv.d.x	fa1, t0
	.loc	1 1 0
	tail	consume
.Lfunc_end0:
	.size	nanbox_tail_half, .-nanbox_tail_half
