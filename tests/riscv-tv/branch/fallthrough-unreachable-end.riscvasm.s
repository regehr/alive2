	.option	norvc
	.text
	.globl	fallthrough_unreachable_end
	.type	fallthrough_unreachable_end,@function
fallthrough_unreachable_end:
	li	a0, 41
.Lreturn:
	addi	a0, a0, 1
	ret
.Ldead:
	li	a0, 0
.Lfunc_end0:
	.size	fallthrough_unreachable_end, .-fallthrough_unreachable_end
