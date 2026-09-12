	.text
	.file	1 "foo.ll"
	.globl	nanbox_call_float_wrong
	.type	nanbox_call_float_wrong,@function
nanbox_call_float_wrong:
	addi	sp, sp, -16
	sd	ra, 8(sp)
	slli	t0, a1, 32
	srli	t0, t0, 32
	fmv.d.x	fa0, t0
	# Source instruction 1 is the call to consume.
	.loc	1 1 0
	call	consume
	ld	ra, 8(sp)
	addi	sp, sp, 16
	ret
.Lfunc_end0:
	.size	nanbox_call_float_wrong, .-nanbox_call_float_wrong
