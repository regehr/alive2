	.option	norvc
	.text
	.globl	jal_zero
	.type	jal_zero,@function
jal_zero:
	li	a0, 42
	jal	zero, .Ltarget
	li	a0, 0
.Ltarget:
	ret
.Lfunc_end0:
	.size	jal_zero, .-jal_zero
